{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE RoleAnnotations #-}
{-# LANGUAGE UndecidableInstances #-}

module Syntax.Parser.Util where

import Control.Applicative (Alternative (..))
import Control.Monad (MonadPlus)
import Control.Monad.Trans (MonadTrans, lift)
import Data.Coerce (coerce)
import Data.Loc (Loc (..), posCol)
import qualified Data.Loc as Loc
import Data.Text (Text)
import Syntax.Concrete (Token (Token))
import Text.Megaparsec (MonadParsec (..), ParseError (..), ParseErrorBundle (..), Pos, PosState (..), ShowErrorComponent, SourcePos (..), State (..), Stream (..), errorOffset, mkPos, setParserState, unPos)
import Text.Megaparsec.Error (parseErrorTextPretty)

------------------------------------------
-- wrap new type for parser
------------------------------------------

type role ParseFunc _ nominal

newtype ParseFunc m a = ParseFunc {parser :: m () -> m a}

(↑) :: (m () -> m a) -> ParseFunc m a
(↑) = coerce

(↓) :: ParseFunc m a -> (m () -> m a)
(↓) = coerce

liftP :: m a -> ParseFunc m a
liftP = (↑) . const

instance Functor m => Functor (ParseFunc m) where
  fmap f p = (↑) $ fmap f . parser p

instance Applicative m => Applicative (ParseFunc m) where
  pure = liftP . pure
  pf <*> pa = (↑) (\sc' -> parser pf sc' <*> parser pa sc')

instance Alternative m => Alternative (ParseFunc m) where
  empty = (↑) (const empty)
  pa <|> pb = (↑) (\sc' -> parser pa sc' <|> parser pb sc')

instance Monad m => Monad (ParseFunc m) where
  pa >>= f = (↑) (\sc -> parser pa sc >>= (\a -> parser (f a) sc))

instance MonadPlus m => MonadPlus (ParseFunc m)

instance MonadTrans ParseFunc where
  lift = liftP

instance MonadParsec e s m => MonadParsec e s (ParseFunc m) where
  parseError = lift . parseError
  label s p = (↑) (label s . parser p)
  hidden p = (↑) (hidden . parser p)
  try p = (↑) (try . parser p)
  lookAhead p = (↑) (lookAhead . parser p)
  notFollowedBy p = (↑) (notFollowedBy . parser p)
  withRecovery f p = (↑) (\sc' -> withRecovery (\err -> parser (f err) sc') (parser p sc'))
  observing p = (↑) (observing . parser p)
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

type SyntacticError = (Loc, String)

getTokenColumn :: Syntax.Concrete.Token e -> Text.Megaparsec.Pos
getTokenColumn (Token s _) = mkPos . posCol $ s

posStateToLoc :: Stream s => PosState s -> Loc
posStateToLoc PosState {pstateOffset, pstateSourcePos = SourcePos {..}} =
  Loc.locOf $ Loc.Pos sourceName (unPos sourceLine) (unPos sourceColumn) pstateOffset

getCurLoc :: (MonadParsec e s m) => m Loc
getCurLoc = do
  st@State {stateOffset, statePosState} <- getParserState
  let pst = reachOffsetNoLine stateOffset statePosState
  setParserState st {statePosState = pst}
  return . posStateToLoc $ pst

getEndLoc :: (MonadParsec e s m) => m Loc
getEndLoc = do
  st@State {stateOffset, statePosState} <- getParserState
  let pst = reachOffsetNoLine stateOffset statePosState
  setParserState st {statePosState = pst}
  return . posStateToLoc $ pst

fromParseErrorBundle :: ShowErrorComponent e => ParseErrorBundle Text e -> [SyntacticError]
fromParseErrorBundle (ParseErrorBundle errs posState) =
  snd $ foldr f (posState, []) errs
  where
    f ::
      ShowErrorComponent e =>
      ParseError Text e ->
      (PosState Text, [SyntacticError]) ->
      (PosState Text, [SyntacticError])
    f err (i, acc) =
      let n = reachOffsetNoLine (errorOffset err) i
       in (n, (posStateToLoc n, parseErrorTextPretty err) : acc)