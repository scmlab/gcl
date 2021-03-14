{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE RoleAnnotations #-}
module Syntax.Parser.Util where

import Data.Text.Lazy (Text)
import Text.Megaparsec (setParserState, MonadParsec(..),  unPos, SourcePos(..), errorOffset, Stream(..), ParseError(..), ParseErrorBundle(..), ShowErrorComponent, PosState(..), State (..))
import Text.Megaparsec.Error (parseErrorTextPretty)
import Data.Loc (Loc(..))
import qualified Data.Loc as Loc
import Control.Monad.Trans (MonadTrans, lift)
import Control.Applicative (Alternative(..))
import Control.Monad (MonadPlus)
import Data.Coerce (coerce)


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

getSourcePos' :: (MonadParsec e s m) => m SourcePos
getSourcePos' = do
  st <- getParserState
  let pst = reachOffsetNoLine (stateOffset st - 1) (statePosState st)
  setParserState st {statePosState = pst}
  return (pstateSourcePos pst)

sourceToLoc :: SourcePos -> Loc
sourceToLoc SourcePos{..} = Loc.locOf $ Loc.Pos sourceName (unPos sourceLine) (unPos sourceColumn) 0

posStateToLoc :: Stream s => PosState s -> Loc
posStateToLoc = Loc.locOf . sourceToLoc . pstateSourcePos

fromParseErrorBundle :: ShowErrorComponent e => ParseErrorBundle Text e -> [SyntacticError]
fromParseErrorBundle (ParseErrorBundle errs posState)=
  snd $ foldr f (posState, []) errs
  where
    f :: ShowErrorComponent e =>
      ParseError Text e
      -> (PosState Text, [SyntacticError])
      -> (PosState Text, [SyntacticError])
    f err (i, acc) =
      let n = reachOffsetNoLine (errorOffset err) i in
        (n, (posStateToLoc n, parseErrorTextPretty err) : acc)