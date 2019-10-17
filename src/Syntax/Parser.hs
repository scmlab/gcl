{-# LANGUAGE OverloadedStrings #-}

module Syntax.Parser where

import Data.Text
import Data.Loc
import Text.Megaparsec hiding (Pos)

import Syntax.Concrete
import Syntax.Lexer

parseProgram :: FilePath -> Text -> Either (ParseErrorBundle Text ()) Program
parseProgram = runParser $ withLoc $ do
  declarations <- many declaration
  statements <- many statement
  return $ Program declarations statements

--------------------------------------------------------------------------------
-- | Statements

statement :: Parser Statement
statement = withLoc $ choice
  [ Skip    <$ symbol "skip"
  , Abort   <$ symbol "abort"
  , Assert  <$ symbol "{" <*> predicate <* symbol "}"
  -- , Assign  <$> variableList <* symbol ":=" <*> expressionList
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
predicate = choice
  [ predicate1
  , withLoc (Implies <$> predicate1 <* symbol "=>" <*> predicate1 <?> "implication")
  ]

predicate1 :: Parser Pred
predicate1 = choice
  [ predicate2
  , withLoc $ Disj <$> predicate2 <*  symbol "||" <*> predicate2
  ]

predicate2 :: Parser Pred
predicate2 = choice
  [ predicate3
  , withLoc $ Conj <$> predicate3 <*  symbol "&&" <*> predicate3
  ]

predicate3 :: Parser Pred
predicate3 = choice
  [ predicate4
  , withLoc $ Neg <$  symbol "not" <*> predicate4
  ]

predicate4 :: Parser Pred
predicate4 = choice
  [ withLoc $ Hole <$  symbol "?"
  , withLoc $ Term <$> expression <*> binaryRelation <*> expression
  ]

--
-- predicate5 :: Parser Pred
-- predicate5 = choice
--   [ withLoc $ Term <$> binaryRelation <*> expression <*> expression
--   ]
--   , Hole    <$  symbol "?"
--   ]


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
  [ Var   <$> variable
  , Lit   <$> literal
  , Op    <$> opName <*> many expression
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
  [ conditionDecl
  , variableDecl
  ]

conditionDecl :: Parser Declaration
conditionDecl = withLoc $ do
  symbol "cond"
  types <- condList
  symbol ":"
  t <- type'
  return $ CondDecl types t

variableDecl :: Parser Declaration
variableDecl = withLoc $ do
  symbol "var"
  vars <- variableList
  symbol ":"
  t <- type'
  return $ VarDecl vars t

--------------------------------------------------------------------------------
-- | Helper functions

withLoc :: Parser (Loc -> a) -> Parser a
withLoc parser = do
  start <- getPos
  result <- parser
  end <- getPos
  return $ result (Loc start end)

  where
    getPos :: Parser Pos
    getPos = do
      offset <- getOffset
      SourcePos filepath line column <- getSourcePos
      return $ Pos filepath (unPos line) (unPos column) offset


--------------------------------------------------------------------------------
-- | Variables and stuff

condition :: Parser Condition
condition = withLoc $ Condition <$> typeName

-- seperated by commas
condList :: Parser [Condition]
condList = sepBy1 condition (symbol ",")

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
