{-# LANGUAGE OverloadedStrings #-}

module Syntax.Parser
  ( parseProgram
  , parsePred
  , parseStmt
  , scan

  , toPos

  , module Syntax.Type
  ) where

import Control.Monad.Combinators.Expr
import Control.Monad.State (lift)
import Control.Monad (void)
import Data.Text.Lazy (Text)
import Data.Loc
import Data.Void
import Text.Megaparsec hiding (Pos, State, ParseError, parse)

import Syntax.Concrete
import Syntax.Parser.Lexer
import Syntax.Parser.Util hiding (withLoc)
import qualified Syntax.Parser.Util as Util
import Syntax.Type



--------------------------------------------------------------------------------
-- | States for source location bookkeeping

type Parser = ParsecT Void TokStream (PosLog Tok)

parse :: Parser a -> FilePath -> Text -> Either SyntaxError a
parse parser filepath raw = do
  let tokenStream = scan filepath raw
  case filterError tokenStream of
    Just e  -> Left (LexicalError e)
    Nothing -> case runPosLog (runParserT parser filepath tokenStream) of
      Left e -> Left (SyntacticError $ fromParseErrorBundle e)
      Right x -> Right x

parseProgram :: FilePath -> Text -> Either SyntaxError Program
parseProgram = parse program

parsePred :: Text -> Either SyntaxError Pred
parsePred = parse predicate "<predicate>"

parseStmt :: Text -> Either SyntaxError Stmt
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
guardedCommand = withLoc $ GdCmd  <$> predicate
                                  <*  (symbol TokGuardArr <?> "->")
                                  <*> some statement

hole :: Parser Stmt
hole = withLocStmt $ Hole <$ (symbol TokQM <?> "?")

spec :: Parser Stmt
spec = do
  ((), start) <- getLoc (symbol TokSpecStart <?> "{!")
  expectNewline <?> "<newline> after a the start of a Spec"
  ignoreNewlines
  stmts <- many statement <?> "statements"
  ((), end)   <- getLoc (symbol TokSpecEnd <?> "!}")
  expectNewline <?> "<newline> after a the end of a Spec"

  return $ Spec stmts start end

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
  ((), start) <- getLoc $ do
    symbol TokNeg
  ignoreNewlines
  return $ \result -> Neg result (start <--> result)

conjunction :: Parser (Pred -> Pred -> Pred)
conjunction = do
  symbol TokConj
  ignoreNewlines
  return $ \x y -> Conj x y (x <--> y)

disjunction :: Parser (Pred -> Pred -> Pred)
disjunction = do
  symbol TokDisj
  ignoreNewlines
  return $ \x y -> Disj x y (x <--> y)

implication :: Parser (Pred -> Pred -> Pred)
implication = do
  symbol TokImpl
  ignoreNewlines
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
  [ Eq  <$ symbol TokEQ
  , LEq <$ symbol TokLTE
  , GEq <$ symbol TokGTE
  , LTh <$ symbol TokLT
  , GTh <$ symbol TokGT
  ]) <?> "binary relation"

--------------------------------------------------------------------------------
-- | Expressions

expressionList :: Parser [Expr]
expressionList = sepBy1 expression (symbol TokComma) <?> "a list of expressions separated by commas"

expression :: Parser Expr
expression = (parens expression <|> term) <?> "expression"

term :: Parser Expr
term = withLoc (choice
  [ try (OpE <$> op <*> parens expressionList)
  , VarE    <$> variable
  , ConstE  <$> constant
  , LitE    <$> literal
  , HoleE   <$  symbol TokQM
  ]) <?> "term"
  where
    op :: Parser Expr
    op = withLoc (VarE <$> variable) <?> "operator"

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
constantDecl = withLocStmt $ do
  symbol TokCon
  types <- constList
  symbol TokSemi <?> "semicolon"
  t <- type'
  return $ ConstDecl types t

variableDecl :: Parser Declaration
variableDecl = withLocStmt $ do
  symbol TokVar
  vars <- variableList
  symbol TokSemi <?> "semicolon"
  t <- type'
  return $ VarDecl vars t

--------------------------------------------------------------------------------
-- | Variables and stuff

constant :: Parser Const
constant = withLoc (Const <$> upperName) <?> "constant"

-- separated by commas
constList :: Parser [Const]
constList = sepBy1 constant (symbol TokComma <?> "comma") <?> "a list of constants separated by commas"

variable :: Parser Var
variable = withLoc (Var <$> lowerName) <?> "variable"

-- separated by commas
variableList :: Parser [Var]
variableList = sepBy1 variable (symbol TokComma <?> "comma") <?> "a list of variables separated by commas"

type' :: Parser Type
type' = withLoc (Type <$> upperName) <?> "type"

--------------------------------------------------------------------------------
-- | Combinators

ignoreNewlines :: Parser ()
ignoreNewlines = void $ many (symbol TokNewline)

expectNewline :: Parser ()
expectNewline = do
  -- see if the latest accepcted token is TokNewline
  t <- lift Util.getLatestToken
  case t of
    Just TokNewline -> return ()
    _ -> void $ some (symbol TokNewline)

-- ignores suffixing newlines
withLoc :: Parser (Loc -> a) -> Parser a
withLoc p = do
  result <- Util.withLoc p
  ignoreNewlines
  return result

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
