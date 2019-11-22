{-# LANGUAGE OverloadedStrings #-}

module Syntax.Parser.Util
  ( PosLog
  , runPosLog
  , markStart, markEnd, updateLoc

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
import Syntax.Parser.TokenStream


--------------------------------------------------------------------------------
-- | Source location bookkeeping

type PosLog = State LocState

type ID = Int
data LocState = LocState
  { latest :: Loc
  , opened :: Set ID      -- waiting to be moved to the "logged" map
                          -- when the starting position of the next token is determined
  , logged :: Map ID Loc  -- waiting to be removed when the ending position is determined
  , index  :: Int         -- for generating fresh IDs
  }

runPosLog :: State LocState a -> a
runPosLog f = evalState f (LocState NoLoc Set.empty Map.empty 0)

markStart :: PosLog ID
markStart = do
  -- get a fresh ID and put it in the "opened" set
  i <- gets index
  modify $ \st -> st
    { index  = succ i
    , opened = Set.insert i (opened st)
    }
  return i

updateLoc :: Loc -> PosLog ()
updateLoc loc = do
  set <- gets opened
  let addedLoc = Map.fromSet (const loc) set
  modify $ \st -> st
    { latest = loc
    , opened = Set.empty
    , logged = Map.union (logged st) addedLoc
    }

markEnd :: ID -> PosLog Loc
markEnd i = do
  end <- gets latest
  loggedPos <- gets logged
  let loc = case Map.lookup i loggedPos of
              Nothing  -> NoLoc
              Just start -> start <--> end
  -- remove it from the "logged" map
  modify $ \st -> st
    { logged = Map.delete i loggedPos
    }
  return loc

--------------------------------------------------------------------------------
-- | Helper functions

type P tok = ParsecT Void (TokenStream (L tok)) PosLog

toPos :: Stream s => PosState s -> Pos
toPos (PosState _ offset (SourcePos filepath line column) _ _) = Pos filepath (unPos line) (unPos column) offset

getLoc :: Streamable tok => P tok a -> P tok (a, Loc)
getLoc parser = do
  i <- lift markStart
  result <- parser
  loc <- lift (markEnd i)
  return (result, loc)

withLoc :: Streamable tok => P tok (Loc -> a) -> P tok a
withLoc parser = do
  (result, loc) <- getLoc parser
  return $ result loc

--------------------------------------------------------------------------------
-- | Combinators

symbol :: (Eq tok, Streamable tok) => tok -> P tok ()
symbol t = do
  L loc _ <- satisfy (\(L _ t') -> t == t')
  lift $ updateLoc loc
  return ()

extract :: Streamable tok => (tok -> Maybe a) -> P tok a
extract f = do
  (s, loc) <- token p Set.empty
  lift $ updateLoc loc
  return s
  where
    p (L loc tok) = case f tok of
      Just s  -> Just (s, loc)
      Nothing -> Nothing
