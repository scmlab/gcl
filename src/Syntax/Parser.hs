{-# LANGUAGE OverloadedStrings #-}

module Syntax.Parser where

import Control.Monad.Combinators.Expr
import Data.Text
import Data.Loc
import Data.Void
import Text.Megaparsec hiding (Pos)

import Syntax.Concrete
import Syntax.Lexer


parseProgram :: FilePath -> Text -> Either (ParseErrorBundle Text Void) Program
parseProgram = parse $ withLoc $ do
  declarations <- many declaration
  statements <- many statement
  eof
  return $ Program declarations statements

--------------------------------------------------------------------------------
-- | Statements

statement :: Parser Statement
statement = withLoc $ choice
  [ Skip    <$ symbol "skip"
  , Abort   <$ symbol "abort"
  , Assert  <$ symbol "{" <*> predicate <* symbol "}"
  , Assign  <$> variableList <* symbol ":=" <*> expressionList
  ]

skip :: Parser Statement
skip = withLoc $ Skip <$ symbol "skip"

abort :: Parser Statement
abort = withLoc $ Abort <$ symbol "abort"

assert :: Parser Statement
assert = withLoc $ Assert <$ symbol "{" <*> predicate <* symbol "}"

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
  [ try (Op <$> opName <*> some expression)
  , Var   <$> variable
  , Const <$> constant
  , Lit   <$> literal
  , HoleE <$  symbol "?"
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

constant :: Parser Constant
constant = withLoc $ Constant <$> typeName

-- seperated by commas
constList :: Parser [Constant]
constList = sepBy1 constant (symbol ",")

variable :: Parser Variable
variable = withLoc $ Variable <$> termName

-- seperated by commas
variableList :: Parser [Variable]
variableList = sepBy1 variable (symbol ",")

type' :: Parser Type
type' = withLoc $ Type <$> typeName

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
