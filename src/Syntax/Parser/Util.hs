{-# LANGUAGE OverloadedStrings #-}

module Syntax.Parser.Util
  ( PosLog
  , runPosLog
  , markStart, markEnd, update, getLatestToken

  , (<??>)

  , toPos
  , getLoc, withLoc

  , symbol, extract
  ) where

import Control.Monad.State
import Data.Loc
import Data.Void
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Map (Map)
import qualified Data.Map as Map
import Text.Megaparsec hiding (Pos, State)
import Language.Lexer.Applicative (TokenStream)
import Syntax.Parser.TokenStream (PrettyToken)

-- | A synonym for 'label' in the form of an operator.

infix 1 <??>

(<??>) :: MonadParsec e s m => m a -> String -> m a
(<??>) = (<??>)
{-# INLINE (<??>) #-}


--------------------------------------------------------------------------------
-- | Source location bookkeeping

type PosLog token = State (LocState token)

type ID = Int
data LocState token = LocState
  { latest :: (Loc, Maybe token)
  , opened :: Set ID      -- waiting to be moved to the "logged" map
                          -- when the starting position of the next token is determined
  , logged :: Map ID Loc  -- waiting to be removed when the ending position is determined
  , index  :: Int         -- for generating fresh IDs
  }

runPosLog :: State (LocState token) a -> a
runPosLog f = evalState f (LocState (NoLoc, Nothing) Set.empty Map.empty 0)

markStart :: PosLog token ID
markStart = do
  -- get a fresh ID and put it in the "opened" set
  i <- gets index
  modify $ \st -> st
    { index  = succ i
    , opened = Set.insert i (opened st)
    }
  return i

update :: Loc -> token -> PosLog token ()
update loc tok = do
  set <- gets opened
  let addedLoc = Map.fromSet (const loc) set
  modify $ \st -> st
    { latest = (loc, Just tok)
    , opened = Set.empty
    , logged = Map.union (logged st) addedLoc
    }

markEnd :: ID -> PosLog token Loc
markEnd i = do
  (end, _) <- gets latest
  loggedPos <- gets logged
  let loc = case Map.lookup i loggedPos of
              Nothing  -> NoLoc
              Just start -> start <--> end
  -- remove it from the "logged" map
  modify $ \st -> st
    { logged = Map.delete i loggedPos
    }
  return loc

getLatestToken :: PosLog token (Maybe token)
getLatestToken = snd <$> gets latest

--------------------------------------------------------------------------------
-- | Helper functions

type P token = ParsecT Void (TokenStream (L token)) (PosLog token)

toPos :: Stream s => PosState s -> Pos
toPos (PosState _ offset (SourcePos filepath line column) _ _) = Pos filepath (unPos line) (unPos column) offset

getLoc :: (Ord tok, Show tok, PrettyToken tok) => P tok a -> P tok (a, Loc)
getLoc parser = do
  i <- lift markStart
  result <- parser
  loc <- lift (markEnd i)
  return (result, loc)

withLoc :: (Ord tok, Show tok, PrettyToken tok) => P tok (Loc -> a) -> P tok a
withLoc parser = do
  (result, loc) <- getLoc parser
  return $ result loc

--------------------------------------------------------------------------------
-- | Combinators

symbol :: (Eq tok, Ord tok, Show tok, PrettyToken tok) => tok -> P tok ()
symbol t = do
  L loc tok <- satisfy (\(L _ t') -> t == t')
  lift $ update loc tok
  return ()

extract :: (Ord tok, Show tok, PrettyToken tok) => (tok -> Maybe a) -> P tok a
extract f = do
  (result, tok, loc) <- token p Set.empty
  lift $ update loc tok
  return result
  where
    p (L loc tok') = case f tok' of
      Just result -> Just (result, tok', loc)
      Nothing -> Nothing
