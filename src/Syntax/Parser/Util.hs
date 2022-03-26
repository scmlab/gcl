{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE DeriveFunctor #-}

module Syntax.Parser.Util where

import           Control.Applicative            ( Alternative(..) )
import           Control.Monad                  ( MonadPlus )
import           Control.Monad.Trans            ( MonadTrans
                                                , lift
                                                )
import           Data.Loc                       ( posCol )
import qualified Data.Loc                      as Loc
import           Data.Text                      ( Text )
import           Syntax.Concrete                ( Token(Token) )
import           Text.Megaparsec                ( MonadParsec(..)
                                                , ParseError(..)
                                                , ParseErrorBundle(..)
                                                , Pos
                                                , PosState(..)
                                                , ShowErrorComponent
                                                , SourcePos(..)
                                                , State(..)
                                                , Stream(..)
                                                , errorOffset
                                                , mkPos
                                                , setParserState
                                                , unPos, TraversableStream (reachOffsetNoLine)
                                                )
import           Text.Megaparsec.Error          ( parseErrorTextPretty )

------------------------------------------
-- wrap new type for parser
------------------------------------------

newtype ParseFunc m a = ParseFunc { unParseFunc :: m () -> m a}
  deriving (Functor)

instance Applicative m => Applicative (ParseFunc m) where
  pure = ParseFunc . const . pure
  pf <*> pa = ParseFunc (\sc' -> unParseFunc pf sc' <*> unParseFunc pa sc')

instance Alternative m => Alternative (ParseFunc m) where
  empty = ParseFunc (const empty)
  pa <|> pb = ParseFunc (\sc' -> unParseFunc pa sc' <|> unParseFunc pb sc')

instance Monad m => Monad (ParseFunc m) where
  pa >>= f =
    ParseFunc (\sc -> unParseFunc pa sc >>= (\a -> unParseFunc (f a) sc))

instance MonadPlus m => MonadPlus (ParseFunc m)

instance MonadTrans ParseFunc where
  lift = ParseFunc . const

instance MonadParsec e s m => MonadParsec e s (ParseFunc m) where
  parseError = lift . parseError
  label s p = ParseFunc (label s . unParseFunc p)
  hidden p = ParseFunc (hidden . unParseFunc p)
  try p = ParseFunc (try . unParseFunc p)
  lookAhead p = ParseFunc (lookAhead . unParseFunc p)
  notFollowedBy p = ParseFunc (notFollowedBy . unParseFunc p)
  withRecovery f p = ParseFunc
    (\sc' -> withRecovery (\err -> unParseFunc (f err) sc') (unParseFunc p sc'))
  observing p = ParseFunc (observing . unParseFunc p)
  eof = lift eof
  token f g = lift (token f g)
  tokens f t = lift (tokens f t)
  takeWhileP s f = lift (takeWhileP s f)
  takeWhile1P s f = lift (takeWhile1P s f)
  takeP s i = lift (takeP s i)
  getParserState = lift getParserState
  updateParserState f = lift (updateParserState f)

------------------------------------------
-- combinator helpers
------------------------------------------

type SyntacticError = (Loc.Pos, String)

getTokenColumn :: Syntax.Concrete.Token e -> Text.Megaparsec.Pos
getTokenColumn (Token s _) = mkPos . posCol $ s

posStateToPos :: Stream s => PosState s -> Loc.Pos
posStateToPos PosState { pstateOffset, pstateSourcePos = SourcePos {..} } =
  Loc.Pos sourceName (unPos sourceLine) (unPos sourceColumn) pstateOffset

getCurPos :: (TraversableStream s, MonadParsec e s m) => m Loc.Pos
getCurPos = do
  st@State { stateOffset, statePosState } <- getParserState
  let pst = reachOffsetNoLine stateOffset statePosState
  setParserState st { statePosState = pst }
  return . posStateToPos $ pst

getEndPos :: (TraversableStream s, MonadParsec e s m) => m Loc.Pos
getEndPos = do
  st@State { stateOffset, statePosState } <- getParserState
  let pst = reachOffsetNoLine stateOffset statePosState
  setParserState st { statePosState = pst }
  return . posStateToPos $ pst

fromParseErrorBundle
  :: ShowErrorComponent e => ParseErrorBundle Text e -> [SyntacticError]
fromParseErrorBundle (ParseErrorBundle errs posState) = snd
  $ foldr f (posState, []) errs
 where
  f
    :: ShowErrorComponent e
    => ParseError Text e
    -> (PosState Text, [SyntacticError])
    -> (PosState Text, [SyntacticError])
  f err (i, acc) =
    let n = reachOffsetNoLine (errorOffset err) i
    in  (n, (posStateToPos n, parseErrorTextPretty err) : acc)
