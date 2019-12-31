{-# LANGUAGE OverloadedStrings #-}

module Syntax.Parser
  ( parseProgram
  , parseSpec
  , SyntacticError
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

import Syntax.Concrete hiding (Fixity(..))
import Syntax.Parser.Lexer
-- import Syntax.Parser.Util hiding (withLoc)
import Syntax.Parser.Util (PosLog, extract)
import qualified Syntax.Parser.Util as Util

import Prelude hiding (Ordering(..))

--------------------------------------------------------------------------------
-- | States for source location bookkeeping

type Parser = ParsecT Void TokStream (PosLog Tok)
type SyntacticError = (Loc, String)

parse :: Parser a -> FilePath -> TokStream -> Either [SyntacticError] a
parse parser filepath tokenStream = do
  -- let tokenStream = scan filepath raw
  -- case filterError tokenStream of
  --   Just e  -> Left [LexicalError e]
  --   Nothing ->
  case Util.runPosLog (runParserT parser filepath tokenStream) of
    Left e -> Left (fromParseErrorBundle e)
    Right x -> Right x

  where
    fromParseErrorBundle :: ShowErrorComponent e
                       => ParseErrorBundle TokStream e
                       -> [SyntacticError]
    fromParseErrorBundle (ParseErrorBundle errors posState)
      = snd $ foldr f (posState, []) errors
      where
        f :: ShowErrorComponent e
          => Mega.ParseError TokStream e
          -> (PosState TokStream, [SyntacticError])
          -> (PosState TokStream, [SyntacticError])
        f err (initial, accum) =
            let (_, next) = reachOffset (errorOffset err) initial
            in (next, (getLoc err, parseErrorTextPretty err):accum)

        getLoc :: ShowErrorComponent e
          => Mega.ParseError TokStream e
          -> Loc
        -- get the Loc of all unexpected tokens
        getLoc (TrivialError _ (Just (Tokens xs)) _) = fold $ fmap locOf xs
        getLoc _ = mempty


parseProgram :: FilePath -> TokStream -> Either [SyntacticError] Program
parseProgram = parse program

parseSpec :: TokStream -> Either [SyntacticError] [Stmt]
parseSpec = parse specContent "<specification>"

-- parseExpr :: Text -> Either [Error] Expr
-- parseExpr = parse predicate "<predicate>"

-- parseStmt :: Text -> Either [Error] Stmt
-- parseStmt = parse statement "<statement>"

program :: Parser Program
program = withLoc $ do
  ignoreNewlines
  declarations <- many declaration <?> "declarations"
  statements <- many statement <?> "statements"
  eof
  return $ Program declarations statements

specContent :: Parser [Stmt]
specContent = do
  ignoreNewlines
  many statement <?> "statements"

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
hole = withLocStmt $ SpecQM <$ (symbol TokQM <?> "?")

spec :: Parser Stmt
spec = withLocStmt $ do
  symbol TokSpecStart <?> "{!"
  expectNewline <?> "<newline> after a the start of a Spec"
  _ <- specContent
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



--------------------------------------------------------------------------------
-- | Expressions

expressionList :: Parser [Expr]
expressionList = sepBy1 expression (symbol TokComma) <?> "a list of expressions separated by commas"

-- operator :: Parser Op
-- operator = withLoc (choice
--   [ EQ  <$ symbol TokEQ
--   , LTE <$ symbol TokLTE
--   , GTE <$ symbol TokGTE
--   , LT  <$ symbol TokLT
--   , GT  <$ symbol TokGT
--
--   , Conj <$ symbol TokConj
--   ]) <?> "operators"

predicate :: Parser Expr
predicate = expression <?> "predicate"

expression :: Parser Expr
expression = makeExprParser termButOp table <?> "expression"
  where
    table :: [[Operator Parser Expr]]
    table = [ [ Postfix application ]
            , [ InfixN compareEQ ]
            , [ Prefix negation ]
            , [ InfixL conjunction ]
            , [ InfixL disjunction ]
            , [ InfixR implication ]
            ]

    application :: Parser (Expr -> Expr)
    application =  do
      terms <- many termButOp
      return $ \func -> do
        let app inner t = App inner t (func <--> t)
        foldl app func terms

    negation :: Parser (Expr -> Expr)
    negation = do
      op <- withLoc (Neg <$ symbol TokNeg)
      return $ \result -> App (Op op (locOf op)) result (op <--> result)

    conjunction :: Parser (Expr -> Expr -> Expr)
    conjunction = do
      op <- withLoc (Conj <$ symbol TokConj)
      return $ \x y -> App (App (Op op (locOf op)) x (x <--> op)) y (x <--> y)

    disjunction :: Parser (Expr -> Expr -> Expr)
    disjunction = do
      op <- withLoc (Disj <$ symbol TokDisj)
      return $ \x y -> App (App (Op op (locOf op)) x (x <--> op)) y (x <--> y)

    implication :: Parser (Expr -> Expr -> Expr)
    implication = do
      op <- withLoc (Implies <$ symbol TokImpl)
      return $ \x y -> App (App (Op op (locOf op)) x (x <--> op)) y (x <--> y)

    compareEQ :: Parser (Expr -> Expr -> Expr)
    compareEQ = do
      op <- withLoc (EQ <$ symbol TokEQ)
      return $ \x y -> App (App (Op op (locOf op)) x (x <--> op)) y (x <--> y)

-- term :: Parser Expr
-- term = parens expression <|> withLoc (choice
--   [ Var    <$> lower
--   , Const  <$> upper
--   , Lit    <$> literal
--   , Op     <$> operator
--   ]) <?> "term"

termButOp :: Parser Expr
termButOp = parens expression <|> withLoc (choice
  [ Var    <$> lower
  , Const  <$> upper
  , Lit    <$> literal
  ]) <?> "term (excluding operators)"

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

-- constant :: Parser Expr
-- constant = withLoc (Const <$> upper) <?> "constant"

-- separated by commas
constList :: Parser [Upper]
constList =
  (sepBy1 upper (symbol TokComma <?> "comma")
    <?> "a list of constants separated by commas")
      <*  ignoreNewlines

-- variable :: Parser Expr
-- variable = withLoc (Var <$> lower) <?> "variable"

-- separated by commas
variableList :: Parser [Lower]
variableList =
    (sepBy1 lower (symbol TokComma <?> "comma")
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

upper :: Parser Upper
upper = withLoc (Upper <$> upperName) <?> "identifier that starts with a uppercase letter"

lowerName :: Parser Text
lowerName = extract p
  where
    p (TokLowerName s) = Just s
    p _ = Nothing

lower :: Parser Lower
lower = withLoc (Lower <$> lowerName) <?> "identifier that starts with a lowercase letter"

integer :: Parser Int
integer = extract p <?> "integer"
  where
    p (TokInt s) = Just s
    p _ = Nothing
