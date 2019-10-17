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
-- | Variables and stuff

condition :: Parser Condition
condition = withLoc $ typeName >>= return . Cond

-- seperated by commas
condList :: Parser [Condition]
condList = sepBy1 condition (symbol ",")

variable :: Parser Variable
variable = withLoc $ termName >>= return . Var

-- seperated by commas
variableList :: Parser [Variable]
variableList = sepBy1 variable (symbol ",")

type' :: Parser Type
type' = withLoc $ typeName >>= return . Type

-- seperated by commas
typeList :: Parser [Type]
typeList = sepBy1 type' (symbol ",")

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
-- | Statements

statement :: Parser Statement
statement = choice
  [ skip
  , abort
  ]

skip :: Parser Statement
skip = withLoc $ do
  symbol "skip"
  return Skip

abort :: Parser Statement
abort = withLoc $ do
  symbol "abort"
  return Abort

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
