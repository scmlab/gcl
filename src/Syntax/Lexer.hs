{-# LANGUAGE OverloadedStrings #-}

module Syntax.Lexer where

import Control.Monad (void)
import Data.Text (Text)
import qualified Data.Text as Text

import qualified Text.Megaparsec.Char.Lexer as L
import qualified Text.Megaparsec.Char as C
import Text.Megaparsec

type Parser = Parsec () Text


skipLineComment :: Parser ()
skipLineComment = L.skipLineComment "--"

skipBlockComment :: Parser ()
skipBlockComment = L.skipBlockComment "{-" "-}"

spaceConsumer :: Parser ()
spaceConsumer = L.space C.space1 skipLineComment skipBlockComment

lexeme :: Parser a -> Parser a
lexeme = L.lexeme spaceConsumer

symbol :: Text -> Parser ()
symbol = void . L.symbol spaceConsumer

-- starts with lowercase alphabets, proceed with alphaNums, underscores and single quotes
termName :: Parser Text
termName = lexeme $ do
  x <- C.lowerChar
  xs <- many $ choice
    [ C.alphaNumChar
    , C.char '\''
    , C.char '_'
    ]
  return $ Text.pack $ x : xs

-- starts with uppercase alphabets, proceed with alphaNums, underscores and single quotes
typeName :: Parser Text
typeName = lexeme $ do
  x <- C.upperChar
  xs <- many $ choice
    [ C.alphaNumChar
    , C.char '\''
    , C.char '_'
    ]
  return $ Text.pack $ x : xs
