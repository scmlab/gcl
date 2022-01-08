{-# LANGUAGE OverloadedStrings #-}

module Syntax.Parser2.Util
  ( PosLog
  , runPosLog
  , getLastToken
  , getLoc
  , withLoc
  , getRange 
  , withRange 
  , symbol
  , symbol'
  , ignore
  , ignoreP
  , extract
  ) where

import           Control.Monad.State
import           Data.Loc
import           Data.Loc.Range
import           Data.Map                       ( Map )
import qualified Data.Map                      as Map
import           Data.Set                       ( Set )
import qualified Data.Set                      as Set
import           Data.Void
import           Language.Lexer.Applicative     ( TokenStream )
import           Syntax.Parser2.TokenStream     ( PrettyToken
                                                , getPos
                                                )
import           Text.Megaparsec         hiding ( Pos
                                                , State
                                                , between
                                                )

--------------------------------------------------------------------------------
-- | Source location bookkeeping

type PosLog token = State (LocState token)

type ID = Int
data LocState token = LocState
  { currentLoc :: Loc         -- current Loc mark
  , lastToken  :: Maybe token  -- the last accepcted token
  , opened     :: Set ID        -- waiting to be moved to the "logged" map
                            -- when the starting position of the next token is determined
  , logged     :: Map ID Loc    -- waiting to be removed when the ending position is determined
  , index      :: Int           -- for generating fresh IDs
  }

runPosLog :: State (LocState token) a -> a
runPosLog f = evalState f (LocState NoLoc Nothing Set.empty Map.empty 0)

getCurrentLoc :: PosLog token Loc
getCurrentLoc = gets currentLoc

getLastToken :: PosLog token (Maybe token)
getLastToken = gets lastToken

-- | Returns an ID (marking the start of the range of some source location)
markStart :: PosLog token ID
markStart = do
  i <- gets index
  modify $ \st -> st { index = succ i, opened = Set.insert i (opened st) }
  return i

-- | Returns the range of some source location.
--   The range starts from where the ID is retreived, and ends from here
markEnd :: ID -> PosLog token Loc
markEnd i = do
  end       <- getCurrentLoc
  loggedPos <- gets logged
  let loc = case Map.lookup i loggedPos of
        Nothing    -> NoLoc
        Just start -> start <--> end
  modify $ \st -> st { logged = Map.delete i loggedPos }
  return loc

-- | Updates the current source location
updateLoc :: Loc -> PosLog token ()
updateLoc loc = do
  set <- gets opened
  let addedLoc = Map.fromSet (const loc) set
  modify $ \st -> st { currentLoc = loc
                     , opened     = Set.empty
                     , logged     = Map.union (logged st) addedLoc
                     }

-- | Updates the latest scanned token
updateToken :: token -> PosLog token ()
updateToken tok = modify $ \st -> st { lastToken = Just tok }

--------------------------------------------------------------------------------
-- | Helper functions

type P token = ParsecT Void (TokenStream (L token)) (PosLog token)

-- Augment a parser with Loc
getLoc :: (Ord tok, Show tok, PrettyToken tok) => P tok a -> P tok (a, Loc)
getLoc parser = do
  i      <- lift markStart
  result <- parser
  loc    <- lift (markEnd i)
  return (result, loc)

-- Augment a parser with Range
getRange :: (Ord tok, Show tok, PrettyToken tok) => P tok a -> P tok (a, Range)
getRange parser = do
  start  <- getPos
  result <- parser
  end    <- getPos
  return (result, Range start end)

withLoc :: (Ord tok, Show tok, PrettyToken tok) => P tok (Loc -> a) -> P tok a
withLoc parser = do
  (result, loc) <- getLoc parser
  return $ result loc

withRange
  :: (Ord tok, Show tok, PrettyToken tok) => P tok (Range -> a) -> P tok a
withRange parser = do
  (result, range) <- getRange parser
  return $ result range

--------------------------------------------------------------------------------
-- | Combinators

-- Create a parser of some symbol (while respecting source locations)
symbol :: (Eq tok, Ord tok, Show tok, PrettyToken tok) => tok -> P tok ()
symbol t = do
  L loc tok <- satisfy (\(L _ t') -> t == t')
  lift $ do
    updateLoc loc
    updateToken tok
  return ()

-- Create a parser of some symbol (while respecting source locations)
symbol' :: (Eq tok, Ord tok, Show tok, PrettyToken tok) => tok -> P tok Loc
symbol' t = do
  L loc tok <- satisfy (\(L _ t') -> t == t')
  lift $ do
    updateLoc loc
    updateToken tok
  return loc

-- Create a parser of some symbol, that doesn't update source locations
-- effectively excluding it from source location tracking
ignore :: (Eq tok, Ord tok, Show tok, PrettyToken tok) => tok -> P tok ()
ignore t = do
  L _ tok <- satisfy ((==) t . unLoc)
  lift $ updateToken tok
  return ()

-- The predicate version of `ignore`
ignoreP
  :: (Eq tok, Ord tok, Show tok, PrettyToken tok) => (tok -> Bool) -> P tok ()
ignoreP p = do
  L _ tok <- satisfy (p . unLoc)
  lift $ updateToken tok
  return ()

-- Useful for extracting values from a Token 
extract :: (Ord tok, Show tok, PrettyToken tok) => (tok -> Maybe a) -> P tok a
extract f = do
  (result, tok, loc) <- token p Set.empty
  lift $ do
    updateLoc loc
    updateToken tok

  return result
 where
  p (L loc tok') = case f tok' of
    Just result -> Just (result, tok', loc)
    Nothing     -> Nothing
