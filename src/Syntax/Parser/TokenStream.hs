{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE RecordWildCards #-}


module Syntax.Parser.TokenStream where

import Data.Loc
import Data.List.NonEmpty (NonEmpty (..))
import Data.Proxy
import Language.Lexer.Applicative
import Text.Megaparsec hiding (Pos)
import Debug.Trace

class Ord tok => Streamable tok where
  showNonEmptyTokens :: NonEmpty (L tok) -> String

instance Ord tok => Ord (TokenStream (L tok)) where
  compare _ _ = EQ

instance (Show tok, Streamable tok) => Stream (TokenStream (L tok)) where
  type Token (TokenStream (L tok)) = L tok
  type Tokens (TokenStream (L tok)) = [L tok]
  tokenToChunk Proxy = tokenToChunk'
  tokensToChunk Proxy = tokensToChunk'
  chunkToTokens Proxy = chunkToTokens'
  chunkLength Proxy = chunkLength'
  chunkEmpty Proxy = chunkEmpty'
  take1_ = take1_'
  takeN_ = takeN_'
  takeWhile_ = takeWhile_'
  showTokens Proxy = showTokens'
  reachOffset = reachOffset'

tokenToChunk' :: L tok -> [L tok]
tokenToChunk' tok = [tok]

tokensToChunk' :: [L tok] -> [L tok]
tokensToChunk' = id

chunkToTokens' :: [L tok] -> [L tok]
chunkToTokens' = id

chunkLength' :: [L tok] -> Int
chunkLength' []     = 0
chunkLength' (x:[]) = tokenWidth x
chunkLength' (_:xs) = chunkLength' xs

chunkEmpty' :: [L tok] -> Bool
chunkEmpty' = (==) 0 . chunkLength'

streamEmpty :: TokenStream (L tok) -> Bool
streamEmpty TsEof                     = True
streamEmpty (TsError _)               = True
streamEmpty (TsToken tok TsEof)       = tokenWidth tok == 0
streamEmpty (TsToken tok (TsError _)) = tokenWidth tok == 0
streamEmpty (TsToken tok rest)        =
    if tokenWidth tok == 0
      then streamEmpty rest
      else False

take1_' :: TokenStream (L tok) -> Maybe (L tok, TokenStream (L tok))
take1_' (TsToken tok rest) = Just (tok, rest)
take1_' _                  = Nothing

takeN_' :: Int -> TokenStream (L tok) -> Maybe ([L tok], TokenStream (L tok))
takeN_' n s
  | n <= 0   = Just ([], s)
  | streamEmpty s = Nothing
  | otherwise     = go n s
  where
    go :: Int -> TokenStream (L tok) -> Maybe ([L tok], TokenStream (L tok))
    go _      TsEof                   = Nothing
    go _      (TsError _)             = Nothing
    go _      (TsToken (L NoLoc _) _) = error "token missing source location"
    go target (TsToken x rest) =
      if target < tokenEndingOffset x
        then Just ([], TsToken x rest) -- not enough to cover the first token
        else case go target rest of
                Just (xs, rest') -> Just (x:xs, rest')
                Nothing          -> Just ([x], TsEof)

takeWhile_' :: (L tok -> Bool) -> TokenStream (L tok) -> ([L tok], TokenStream (L tok))
takeWhile_' p stream = case take1_' stream of
  Nothing        -> ([], stream)
  Just (x, rest) ->
    if p x
      then let (xs, rest') = takeWhile_' p rest in (x:xs, rest')
      else ([], stream)

showTokens' :: Streamable tok => NonEmpty (L tok) -> String
showTokens' = showNonEmptyTokens

reachOffset' :: Int
             -> PosState (TokenStream (L tok))
             -> (String, PosState (TokenStream (L tok)))
reachOffset' n posState = case takeN_' n (pstateInput posState) of
  -- the stream is empty
  Nothing -> ("<empty line>", posState)
  Just ([], _) -> ("<empty line>", posState)

  Just (xs, rest) -> ("<not yet implemented>", posState')
    where
      latestToken = last xs

      endingPos = case latestToken of
                    L NoLoc _ -> error "missing source location"
                    L (Loc _ end) _ -> end

      -- updated 'PosState'
      posState' = PosState
        { pstateInput = rest
        , pstateOffset = tokenEndingOffset (last xs)
        , pstateSourcePos = toSourcePos endingPos
        , pstateTabWidth = pstateTabWidth posState
        , pstateLinePrefix = pstateLinePrefix posState
        }

--------------------------------------------------------------------------------
-- Helpers

-- | Get the offset of the ending position of a Loc
-- locOffset :: Loc -> Int
-- locOffset NoLoc = 0
-- locOffset (Loc _ end) = posCoff end + 1

tokenWidth :: L tok -> Int
tokenWidth (L NoLoc _) = 0
tokenWidth (L (Loc start end) _) = posCoff end - posCoff start + 1

-- | Get the ending offset of the next token
tokenEndingOffset :: L tok -> Int
tokenEndingOffset (L NoLoc _) = 0
tokenEndingOffset (L (Loc _ end) _) = posCoff end + 1

-- | Get the "width" of the next token
nextTokenWidth :: TokenStream (L tok) -> Int
nextTokenWidth (TsToken tok _) = tokenWidth tok
nextTokenWidth _               = 0


toSourcePos :: Pos -> SourcePos
toSourcePos (Pos filename line column _) =
    SourcePos filename (mkPos line) (mkPos column)
