{-# LANGUAGE OverloadedStrings #-}

module Syntax.Parser
  ( parseProgram
  , parsePred
  , parseStmt
  , scan
  ) where

import Control.Monad.Combinators.Expr
import Control.Monad.State (lift)
import Control.Monad (void)
import Data.Text.Lazy (Text)
import Data.Loc
import Data.Void
import Data.Foldable (fold)
import Text.Megaparsec hiding (Pos, State, ParseError, parse)
import qualified Text.Megaparsec as Mega

import Syntax.Concrete
import Syntax.Parser.Lexer
-- import Syntax.Parser.Util hiding (withLoc)
import Syntax.Parser.Util (PosLog, extract)
import qualified Syntax.Parser.Util as Util
import Type

import Prelude hiding (Ordering(..))



--------------------------------------------------------------------------------
-- | States for source location bookkeeping

type Parser = ParsecT Void TokStream (PosLog Tok)

parse :: Parser a -> FilePath -> Text -> Either [Error] a
parse parser filepath raw = do
  let tokenStream = scan filepath raw
  case filterError tokenStream of
    Just e  -> Left [LexicalError e]
    Nothing -> case Util.runPosLog (runParserT parser filepath tokenStream) of
      Left e -> Left (fromParseErrorBundle e)
      Right x -> Right x

  where
    fromParseErrorBundle :: ShowErrorComponent e
                       => ParseErrorBundle TokStream e
                       -> [Error]
    fromParseErrorBundle (ParseErrorBundle errors posState)
      = snd $ foldr f (posState, []) errors
      where
        f :: ShowErrorComponent e
          => Mega.ParseError TokStream e
          -> (PosState TokStream, [Error])
          -> (PosState TokStream, [Error])
        f err (initial, accum) =
            let (_, next) = reachOffset (errorOffset err) initial
            in (next, (SyntacticError (getLoc err) (parseErrorTextPretty err)):accum)

        getLoc :: ShowErrorComponent e
          => Mega.ParseError TokStream e
          -> Loc
        -- get the Loc of all unexpected tokens
        getLoc (TrivialError _ (Just (Tokens xs)) _) = fold $ fmap locOf xs
        getLoc _ = mempty


parseProgram :: FilePath -> Text -> Either [Error] Program
parseProgram = parse program

parsePred :: Text -> Either [Error] Pred
parsePred = parse predicate "<predicate>"

parseStmt :: Text -> Either [Error] Stmt
parseStmt = parse statement "<statement>"

program :: Parser Program
program = withLoc $ do
  ignoreNewlines
  declarations <- many declaration <?> "declarations"
  statements <- many statement <?> "statements"
  eof
  return $ Program declarations statements

--------------------------------------------------------------------------------
-- | Stmts

statement :: Parser Stmt
statement = choice
  [ try assign
  , abort
  , try assertWithBnd
  , spec
  , assert
  , skip
  , repetition
  , selection
  , hole
  ] <?> "statement"


skip :: Parser Stmt
skip = withLocStmt $ Skip <$ symbol TokSkip

abort :: Parser Stmt
abort = withLocStmt $ Abort <$ symbol TokAbort

assert :: Parser Stmt
assert = withLocStmt $ Assert <$> braces predicate

assertWithBnd :: Parser Stmt
assertWithBnd = withLocStmt $ braces $ AssertWithBnd
  <$> predicate
  <*  (symbol TokComma <?> "comma")
  <*  (symbol TokBnd <?> "bnd")
  <*  (symbol TokSemi <?> "semicolon")
  <*> expression

assign :: Parser Stmt
assign = withLocStmt $
  Assign  <$> variableList
          <*  (symbol TokAssign <?> ":=")
          <*> expressionList

repetition :: Parser Stmt
repetition = withLocStmt $
  Do  <$  (symbol TokDo <?> "do")
      <*> guardedCommands
      <*  (symbol TokOd <?> "od")

selection :: Parser Stmt
selection = withLocStmt $
  If  <$  (symbol TokIf <?> "if")
      <*> guardedCommands
      <*  (symbol TokFi <?> "fi")

guardedCommands :: Parser [GdCmd]
guardedCommands = sepBy1 guardedCommand (symbol TokGuardBar <?> "|")

guardedCommand :: Parser GdCmd
guardedCommand = withLocStmt $
  GdCmd <$> predicate
        <*  (symbol TokGuardArr <?> "->")
        <*> some statement

hole :: Parser Stmt
hole = withLocStmt $ Hole <$ (symbol TokQM <?> "?")

spec :: Parser Stmt
spec = withLocStmt $ do
  symbol TokSpecStart <?> "{!"
  expectNewline <?> "<newline> after a the start of a Spec"
  -- stmts <- many statement <?> "statements"
  _ <- takeWhileP (Just "anything other than '!}'") isTokSpecEnd
  symbol TokSpecEnd <?> "!}"
  expectNewline <?> "<newline> after a the end of a Spec"

  return $ Spec

  where
    isTokSpecEnd :: L Tok -> Bool
    isTokSpecEnd (L _ TokSpecEnd) = False
    isTokSpecEnd _ = True

--------------------------------------------------------------------------------
-- | Predicates

predicate :: Parser Pred
predicate = makeExprParser predTerm table <?> "predicate"
  where
    table :: [[Operator Parser Pred]]
    table = [ [ Prefix negation ]
            , [ InfixL conjunction ]
            , [ InfixL disjunction ]
            , [ InfixR implication ]
            ]


negation :: Parser (Pred -> Pred)
negation = do
  ((), start) <- Util.getLoc $ do
    symbol TokNeg
  return $ \result -> Neg result (start <--> result)

conjunction :: Parser (Pred -> Pred -> Pred)
conjunction = do
  symbol TokConj
  return $ \x y -> Conj x y (x <--> y)

disjunction :: Parser (Pred -> Pred -> Pred)
disjunction = do
  symbol TokDisj
  return $ \x y -> Disj x y (x <--> y)

implication :: Parser (Pred -> Pred -> Pred)
implication = do
  symbol TokImpl
  return $ \x y -> Implies x y (x <--> y)


predTerm :: Parser Pred
predTerm =  parens predicate
        <|> (withLoc $ choice
              [ HoleP <$  symbol TokQM
              , Lit True <$ symbol TokTrue
              , Lit False <$ symbol TokFalse
              , Term <$> expression <*> binaryRelation <*> expression
              ])

binaryRelation :: Parser BinRel
binaryRelation = withLoc (choice
  [ EQ  <$ symbol TokEQ
  , LTE <$ symbol TokLTE
  , GTE <$ symbol TokGTE
  , LT  <$ symbol TokLT
  , GT  <$ symbol TokGT
  ]) <?> "binary relation"

--------------------------------------------------------------------------------
-- | Expressions

expressionList :: Parser [Expr]
expressionList = sepBy1 expression (symbol TokComma) <?> "a list of expressions separated by commas"

expression :: Parser Expr
expression = foldAp <$> expr <*> many expr <?> "expression"
  where
    foldAp :: Expr -> [Expr] -> Expr
    foldAp f [] = f
    foldAp f (x:xs) =
      foldAp (ApE f x (locOf f <--> locOf x)) xs

    expr :: Parser Expr
    expr = parens expression <|> term

    term :: Parser Expr
    term = withLoc (choice
      [ VarE    <$> variable
      , ConstE  <$> constant
      , LitE    <$> literal
      , HoleE   <$  symbol TokQM
      ]) <?> "term"
  -- where
  --   op :: Parser Expr
    -- op = withLoc (VarE <$> variable) <?> "operator"

literal :: Parser Lit
literal = choice
  [ Bol True  <$  symbol TokTrue
  , Bol False <$  symbol TokFalse
  , Num       <$> integer
  ] <?> "literal"


--------------------------------------------------------------------------------
-- | Declarations

declaration :: Parser Declaration
declaration = choice
  [ constantDecl
  , variableDecl
  ] <?> "declaration"

constantDecl :: Parser Declaration
constantDecl = withLoc $ do
  symbol TokCon
  types <- constList
  symbol TokSemi <?> "semicolon"
  t <- type'
  expectNewline <?> "<newline> after a declaration"
  return $ ConstDecl types t

variableDecl :: Parser Declaration
variableDecl = withLoc $ do
  symbol TokVar
  vars <- variableList
  symbol TokSemi <?> "semicolon"
  t <- type'
  expectNewline <?> "<newline> after a declaration"
  return $ VarDecl vars t

--------------------------------------------------------------------------------
-- | Variables and stuff

constant :: Parser Const
constant = withLoc (Const <$> upperName) <?> "constant"

-- separated by commas
constList :: Parser [Const]
constList =
  (sepBy1 constant (symbol TokComma <?> "comma")
    <?> "a list of constants separated by commas")
      <*  ignoreNewlines

variable :: Parser Var
variable = withLoc (Var <$> lowerName) <?> "variable"

-- separated by commas
variableList :: Parser [Var]
variableList =
    (sepBy1 variable (symbol TokComma <?> "comma")
      <?> "a list of variables separated by commas")
        <*  ignoreNewlines

type' :: Parser Type
type' = withLoc (Type <$> upperName) <?> "type"

--------------------------------------------------------------------------------
-- | Combinators

ignoreNewlines :: Parser ()
ignoreNewlines = void $ many (Util.ignore TokNewline)

expectNewline :: Parser ()
expectNewline = do
  -- see if the latest accepcted token is TokNewline
  t <- lift Util.getLastToken
  case t of
    Just TokNewline -> return ()
    _ -> void $ some (Util.ignore TokNewline)

symbol :: Tok -> Parser ()
symbol t = do
  Util.symbol t
  ignoreNewlines

withLoc :: Parser (Loc -> a) -> Parser a
withLoc = Util.withLoc

-- followed by at least 1 newline
withLocStmt :: Parser (Loc -> a) -> Parser a
withLocStmt p = do
  result <- Util.withLoc p
  expectNewline <?> "<newline> after a statement"
  return result

parens :: Parser a -> Parser a
parens = between
  (symbol TokParenStart <?> "left parenthesis")
  (symbol TokParenEnd <?> "right parenthesis")

braces :: Parser a -> Parser a
braces = between
  (symbol TokBraceStart <?> "left brace")
  (symbol TokBraceEnd <?> "right brace")

upperName :: Parser Text
upperName = extract p
  where
    p (TokUpperName s) = Just s
    p _ = Nothing

lowerName :: Parser Text
lowerName = extract p
  where
    p (TokLowerName s) = Just s
    p _ = Nothing

integer :: Parser Int
integer = extract p <?> "integer"
  where
    p (TokInt s) = Just s
    p _ = Nothing
