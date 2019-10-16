{-# LANGUAGE OverloadedStrings #-}

module Syntax.Lexer where

import Control.Monad (void)
import Data.Text (Text)

import qualified Text.Megaparsec.Char.Lexer as L
import qualified Text.Megaparsec.Char as C
import Text.Megaparsec (Parsec, Token, Tokens)

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
