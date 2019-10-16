{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ConstraintKinds #-}

module Syntax.Lexer where

import Control.Monad (void)

import qualified Text.Megaparsec.Char.Lexer as L
import qualified Text.Megaparsec.Char as C
import Text.Megaparsec (MonadParsec, Token, Tokens)

type Lexer e s m = (MonadParsec e s m, Token s ~ Char, Tokens s ~ String)

skipLineComment :: Lexer e s m => m ()
skipLineComment = L.skipLineComment "--"

skipBlockComment :: Lexer e s m => m ()
skipBlockComment = L.skipBlockComment "{-" "-}"

spaceConsumer :: Lexer e s m => m ()
spaceConsumer = L.space C.space1 skipLineComment skipBlockComment

-- keyword :: MonadParsec e s m => m Pos

lexeme :: Lexer e s m => m a -> m a
lexeme = L.lexeme spaceConsumer

symbol :: Lexer e s m => String -> m ()
symbol = void . L.symbol spaceConsumer

integer :: Lexer e s m => m Integer
integer = lexeme L.decimal
