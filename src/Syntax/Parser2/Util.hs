{-# LANGUAGE OverloadedStrings #-}

module Syntax.Parser2.Util
  ( M
  , Parser
  , runM
  , getLastToken
  , getLoc
  , withLoc
  , getRange
  , withRange
  , symbol
  , anySymbol
  , extract
  , ignore
  , ignoreP
  , getLeftBound
  , alignWith
  , indentTo
  ) where

import           Control.Monad.State
import           Data.Loc
import           Data.Loc.Range
import           Data.Map                       ( Map )
import qualified Data.Map                      as Map
import           Data.Set                       ( Set )
import qualified Data.Set                      as Set
import           Data.Void
import           Syntax.Parser2.Lexer           ( Tok
                                                , TokStream
                                                )
import           Text.Megaparsec         hiding ( Pos
                                                , State
                                                , between
                                                )
import qualified Data.List.NonEmpty as NEL

--------------------------------------------------------------------------------
-- | Source location bookkeeping

type M = State Bookkeeping

type ID = Int
data Bookkeeping = Bookkeeping
  { currentLoc :: Loc         -- current Loc mark
  , lastToken  :: Maybe Tok   -- the last accepcted token
  , opened     :: Set ID      -- waiting to be moved to the "logged" map
                              -- when the starting position of the next token is determined
  , logged     :: Map ID Loc  -- waiting to be removed when the ending position is determined
  , index      :: Int         -- for generating fresh IDs
  , indentStack:: [Pos]       -- Recording required indentation while parsing.
  }

runM :: State Bookkeeping a -> a
runM f = evalState f (Bookkeeping NoLoc Nothing Set.empty Map.empty 0 [])

getCurrentLoc :: M Loc
getCurrentLoc = gets currentLoc

getLastToken :: M (Maybe Tok)
getLastToken = gets lastToken

-- | Returns an ID (marking the start of the range of some source location)
markStart :: M ID
markStart = do
  i <- gets index
  modify $ \st -> st { index = succ i, opened = Set.insert i (opened st) }
  return i

-- | Returns the range of some source location.
--   The range starts from where the ID is retreived, and ends from here
markEnd :: ID -> M Loc
markEnd i = do
  end       <- getCurrentLoc
  loggedPos <- gets logged
  let loc = case Map.lookup i loggedPos of
        Nothing    -> NoLoc
        Just start -> start <--> end
  modify $ \st -> st { logged = Map.delete i loggedPos }
  return loc

-- | Updates the current source location
updateLoc :: Loc -> M ()
updateLoc loc = do
  set <- gets opened
  let addedLoc = Map.fromSet (const loc) set
  modify $ \st -> st { currentLoc = loc
                     , opened     = Set.empty
                     , logged     = Map.union (logged st) addedLoc
                     }

-- | Updates the latest scanned token
updateToken :: Tok -> M ()
updateToken tok = modify $ \st -> st { lastToken = Just tok }

--------------------------------------------------------------------------------
-- | Helper functions

type Parser = ParsecT Void TokStream M

getLoc :: Parser a -> Parser (a, Loc)
getLoc parser = do
  i      <- lift markStart
  result <- parser
  loc    <- lift (markEnd i)
  return (result, loc)

getRange :: Parser a -> Parser (a, Range)
getRange parser = do
  (result, loc) <- getLoc parser
  case loc of
    NoLoc         -> error "NoLoc when getting srcloc info from a token"
    Loc start end -> return (result, Range start end)

withLoc :: Parser (Loc -> a) -> Parser a
withLoc parser = do
  (result, loc) <- getLoc parser
  return $ result loc

withRange :: Parser (Range -> a) -> Parser a
withRange parser = do
  (result, range) <- getRange parser
  return $ result range

-- next functions are for indentation

getLeftBound :: Parser a -> Parser (a, Pos)
getLeftBound p = do
        (r, Range s _) <- getRange p
        return (r, s)

lastIndentReq :: Parser (Maybe Pos)
lastIndentReq = do
  stack <- gets indentStack
  case stack of
    [] -> return Nothing
    p : _ -> return (Just p)

insertIndentReq :: Pos -> Parser ()
insertIndentReq pos = do
  stack <- gets indentStack
  modify $ \st->st { indentStack = pos:stack }

popIndentReq :: Parser (Maybe Pos)
popIndentReq = do
  stack <- gets indentStack
  case stack of
    [] -> return Nothing
    p : ps -> do
      modify $ \st->st {indentStack = ps}
      return (Just p)

-- | See this part of 'symbol' as an example:
-- > do
-- >   ir <- lastIndentReq
-- >   L loc tok <- satisfy (\(L loc t') -> t == t' && (loc `fitsIndentReq` ir) )
--   ...
fitsIndentReq :: Loc -> Maybe Pos -> Bool
fitsIndentReq (Loc start _) indentReq =
  case indentReq of
    Nothing -> True
    Just leftbound -> posCol start > posCol leftbound
fitsIndentReq NoLoc _ = True
--------------------------------------------------------------------------------
-- | Combinators

-- Parsing with bookkeeping actions: symbol, anySymbol, extract
-- Any parser should be built upon these combinators.

-- Create a parser of some symbol (while respecting source locations)
symbol :: Tok -> Parser Loc
symbol t = do
  ir <- lastIndentReq
  L loc tok <- satisfy (\(L loc t') -> t == t' && (loc `fitsIndentReq` ir) )
  lift $ do
    updateLoc loc
    updateToken tok
  return loc

anySymbol :: Parser Loc
anySymbol = do
  ir <- lastIndentReq
  L loc tok <- satisfy (\(L loc _) -> loc `fitsIndentReq` ir )
  lift $ do
    updateLoc loc
    updateToken tok
  return loc

-- Useful for extracting values from a Token 
extract :: (Tok -> Maybe a) -> Parser a
extract f = do
  ir <- lastIndentReq
  let predicate (L loc tok') = case f tok' of
        Just result -> if loc `fitsIndentReq` ir
          then Just (result, tok', loc)
          else Nothing
        Nothing     -> Nothing
  (result, tok, loc) <- token predicate Set.empty
  lift $ do
    updateLoc loc
    updateToken tok

  return result

-- Create a parser of some symbol, that doesn't update source locations
-- effectively excluding it from source location tracking
ignore :: Tok -> Parser ()
ignore t = do
  L _ tok <- satisfy ((==) t . unLoc)
  lift $ updateToken tok
  return ()

-- The predicate version of `ignore`
ignoreP :: (Tok -> Bool) -> Parser ()
ignoreP p = do
  L _ tok <- satisfy (p . unLoc)
  lift $ updateToken tok
  return ()


-- combinators for indentation
-- the design principle of indentation constraints is to reduce ambiguity

-- | The input Parser should be built from either 'symbol' or 'extract', or it'll raise NoLoc error when doing 'getRange'.
-- This is because the loc being extracted inside 'getRange'(and 'getLoc') is generated by bookkeeping actions: updateLoc,
-- which is only been done in 'symbol' and 'extract'.
-- So parsers supported by Megaparsec like 'anySingle' cannot be used here.
alignWith :: Pos -> Parser a -> Parser a
alignWith pos p = do
  (r, leftBound) <- getLeftBound p
  if posCol pos == posCol leftBound
    then return r
    else failure Nothing (Set.fromList [Label (NEL.fromList "aligned symbol")])

-- | The input Parser should be built from either 'symbol' or 'extract'.
indentTo :: Pos -> Parser a -> Parser a
indentTo pos p = do
  insertIndentReq pos
  r <- p
  _ <- popIndentReq
  return r

