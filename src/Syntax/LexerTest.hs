{-# LANGUAGE OverloadedStrings #-}

module Syntax.LexerTest where

import Language.Lexer.Applicative
import Text.Regex.Applicative
import Data.Char hiding (Space)
import Data.Text (Text, pack)
import Data.Loc (L)

data Tok = A | B | C | W
  deriving (Eq, Ord, Show)

tokRE :: RE Char Tok
tokRE
   =  A <$ "<->"
  <|> B <$ "BB"
  <|> C <$ "C"

showToken :: Tok -> String
showToken W = " "
showToken x = show x

whitespaceRE :: RE Char Tok
whitespaceRE = matchWhen isSpace W
  where
    matchWhen :: (s -> Bool) -> a -> RE s a
    matchWhen p symbol = msym (\t -> if p t then Just symbol else Nothing)

lexer :: Lexer Tok
lexer = mconcat
  [ token       (longest tokRE)
  , whitespace  (longest whitespaceRE)
  ]

type TStream = TokenStream (L Tok)

raw :: String
raw = "<->  BB  C C"

run :: TStream
run = runLexer lexer "<file>" raw
