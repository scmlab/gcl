-- {-# LANGUAGE TypeFamilies #-}

module Syntax.Parser where

-- import qualified Text.Megaparsec.Char.Lexer as L
-- import qualified Text.Megaparsec.Char as C
import Data.Loc
import Text.Megaparsec hiding (Pos)

import Syntax.Concrete
import Syntax.Lexer
--
type Parser = Parsec () String

parseProgram :: FilePath -> String -> Either (ParseErrorBundle String ()) [Statement]
parseProgram = runParser $ do
  many statement

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
