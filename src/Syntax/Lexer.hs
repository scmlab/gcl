{-# LANGUAGE OverloadedStrings #-}

module Syntax.Lexer where

import Control.Monad (void)
import Data.Text (Text)
import Data.Void
import qualified Data.Text as Text

import qualified Text.Megaparsec.Char.Lexer as L
import qualified Text.Megaparsec.Char as C
import Text.Megaparsec

type Parser = Parsec Void Text


--------------------------------------------------------------------------------
-- | Space and newline

skipLineComment :: Parser ()
skipLineComment = L.skipLineComment "--"

skipBlockComment :: Parser ()
skipBlockComment = L.skipBlockComment "{-" "-}"

spaceConsumer :: Parser ()
spaceConsumer = L.space C.space1 skipLineComment skipBlockComment

--------------------------------------------------------------------------------
-- | Lexemes

lexeme :: Parser a -> Parser a
lexeme = L.lexeme spaceConsumer

symbol :: Text -> Parser ()
symbol s = void $ lexeme (C.string s)

keyword :: Text -> Parser ()
keyword s = void $ lexeme (C.string s <* notFollowedBy identifierProceedingChar)

integer :: Parser Int
integer = lexeme L.decimal

--------------------------------------------------------------------------------
-- | Identifiers

keywords :: [Text]
keywords =
  [ "skip"
  , "abort"
  , "do", "od"
  , "if", "fi"
  , "bnd"
  ]

-- can be either a alphanumeric, underscore or a single quote
identifierProceedingChar :: Parser Char
identifierProceedingChar = choice
  [ C.alphaNumChar
  , C.char '\''
  , C.char '_'
  ]

-- starts with lowercase alphabet
identifier :: Parser Text
identifier = lexeme $ do
  x <- C.lowerChar <?> "should be lowercase"
  xs <- many identifierProceedingChar

  let result = Text.pack $ x : xs
  if result `elem` keywords
    then empty
    else return result

-- starts with uppercase alphabet
identifierUpper :: Parser Text
identifierUpper = lexeme $ do
  x <- C.upperChar
  xs <- many identifierProceedingChar

  let result = Text.pack $ x : xs
  if result `elem` keywords
    then empty
    else return result

-- starts with lowercase alphabets, proceed with alphaNums, underscores and single quotes
opName :: Parser Text
opName = identifier
