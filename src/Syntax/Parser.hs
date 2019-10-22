{-# LANGUAGE OverloadedStrings #-}

module Syntax.Parser where

import Control.Monad.Combinators.Expr
import Data.Text
import Data.Loc
import Data.Void
import Text.Megaparsec hiding (Pos)

import Syntax
import Syntax.Lexer


parseProgram :: FilePath -> Text -> Either (ParseErrorBundle Text Void) Program
parseProgram = parse $ withLoc $ do
  declarations <- many declaration
  statements <- many statement
  eof
  return $ Program declarations statements

--------------------------------------------------------------------------------
-- | Stmts

statement :: Parser Stmt
statement = choice
  [ try assign
  , abort
  , assert
  , skip
  , repetition
  , selection
  ]

skip :: Parser Stmt
skip = withLoc $ Skip <$ symbol "skip"

abort :: Parser Stmt
abort = withLoc $ Abort <$ symbol "abort"

assert :: Parser Stmt
assert = withLoc $ Assert <$> braces predicate

assign :: Parser Stmt
assign = withLoc $ Assign <$> variableList <* symbol ":=" <*> expressionList

repetition :: Parser Stmt
repetition = withLoc $ Do <$  symbol "do"
                          <*> braces expression
                          <*> some guardedCommand
                          <*  symbol "od"

selection :: Parser Stmt
selection = withLoc $ If  <$  symbol "if"
                          <*> some guardedCommand
                          <*  symbol "fi"

guardedCommand :: Parser GdCmd
guardedCommand = withLoc $ GdCmd  <$  symbol "|"
                                  <*> predicate
                                  <*  symbol "->"
                                  <*> some statement

--------------------------------------------------------------------------------
-- | Predicates

predicate :: Parser Pred
predicate = makeExprParser term table <?> "predicate"

negation :: Parser (Pred -> Pred)
negation = do
  start <- getPos
  symbol "not"
  return $ \result -> Neg result (start <--> result)

conjunction :: Parser (Pred -> Pred -> Pred)
conjunction = do
  symbol "&&"
  return $ \x y -> Conj x y (x <--> y)

disjunction :: Parser (Pred -> Pred -> Pred)
disjunction = do
  symbol "||"
  return $ \x y -> Disj x y (x <--> y)

implication :: Parser (Pred -> Pred -> Pred)
implication = do
  symbol "=>"
  return $ \x y -> Implies x y (x <--> y)

table :: [[Operator Parser Pred]]
table = [ [ Prefix negation ]
        , [ InfixL conjunction ]
        , [ InfixL disjunction ]
        , [ InfixR implication ]
        ]

term :: Parser Pred
term = parens predicate
  <|> (withLoc $ Hole <$  symbol "?")
  <|> (withLoc $ Term <$> expression <*> binaryRelation <*> expression)
  -- <?> "term"

binaryRelation :: Parser BinRel
binaryRelation = withLoc $ choice
  [ Eq  <$ symbol "="
  , LEq <$ symbol "<="
  , GEq <$ symbol ">="
  , LTh <$ symbol "<"
  , GTh <$ symbol ">"
  ]

--------------------------------------------------------------------------------
-- | Expressions

expression :: Parser Expr
expression = withLoc $ choice
  [ try (OpE <$> opName <*> some expression)
  , VarE    <$> variable
  , ConstE  <$> constant
  , LitE    <$> literal
  , HoleE   <$  symbol "?"
  ]

expressionList :: Parser [Expr]
expressionList = sepBy1 expression (symbol ",")

literal :: Parser Lit
literal = choice
  [ Bol True  <$  symbol "true"
  , Bol False <$  symbol "false"
  , Num       <$> integer
  ]


--------------------------------------------------------------------------------
-- | Declarations

declaration :: Parser Declaration
declaration = choice
  [ constantDecl
  , variableDecl
  ]

constantDecl :: Parser Declaration
constantDecl = withLoc $ do
  symbol "con"
  types <- constList
  symbol ":"
  t <- type'
  return $ ConstDecl types t

variableDecl :: Parser Declaration
variableDecl = withLoc $ do
  symbol "var"
  vars <- variableList
  symbol ":"
  t <- type'
  return $ VarDecl vars t

--------------------------------------------------------------------------------
-- | Variables and stuff

constant :: Parser Const
constant = withLoc $ Const <$> identifierUpper

-- seperated by commas
constList :: Parser [Const]
constList = sepBy1 constant (symbol ",")

variable :: Parser Var
variable = withLoc $ Var <$> identifier

-- seperated by commas
variableList :: Parser [Var]
variableList = sepBy1 variable (symbol ",")

type' :: Parser Type
type' = withLoc $ Type <$> identifierUpper

-- seperated by commas
typeList :: Parser [Type]
typeList = sepBy1 type' (symbol ",")


--------------------------------------------------------------------------------
-- | Helper functions

getPos :: Parser Pos
getPos = do
  offset <- getOffset
  SourcePos filepath line column <- getSourcePos
  return $ Pos filepath (unPos line) (unPos column) offset

withLoc :: Parser (Loc -> a) -> Parser a
withLoc parser = do
  start <- getPos
  result <- parser
  end <- getPos
  return $ result (Loc start end)

parens :: Parser a -> Parser a
parens = between (symbol "(") (symbol ")")

braces :: Parser a -> Parser a
braces = between (symbol "{") (symbol "}")
