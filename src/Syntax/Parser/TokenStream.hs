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

class Ord tok => Streamable tok where
  showNonEmptyTokens :: NonEmpty (L tok) -> String

instance Ord tok => Ord (TokenStream (L tok)) where
  compare _ _ = EQ

instance (Streamable tok) => Stream (TokenStream (L tok)) where
  type Token (TokenStream (L tok)) = L tok
  type Tokens (TokenStream (L tok)) = [L tok]
  tokenToChunk Proxy tok = [tok]
  tokensToChunk Proxy = id
  chunkToTokens Proxy = id
  chunkLength Proxy = chunkLength'
  chunkEmpty Proxy = chunkEmpty'
  take1_ = take1_'
  takeN_ = takeN_'
  takeWhile_ = takeWhile_'
  showTokens Proxy = showNonEmptyTokens
  reachOffset = reachOffset'

chunkLength' :: [L tok] -> Int
chunkLength' = length

chunkEmpty' :: [L tok] -> Bool
chunkEmpty' = (==) 0 . chunkLength'

streamEmpty :: TokenStream (L tok) -> Bool
streamEmpty (TsToken _ _) = False
streamEmpty TsEof         = True
streamEmpty (TsError _)   = True

take1_' :: TokenStream (L tok) -> Maybe (L tok, TokenStream (L tok))
take1_' (TsToken tok rest) = Just (tok, rest)
take1_' _                  = Nothing

takeN_' :: Int -> TokenStream (L tok) -> Maybe ([L tok], TokenStream (L tok))
takeN_' n s
  | n <= 0        = Just ([], s)
  | streamEmpty s = Just ([], s)
  | otherwise     = Just (jump n s)
  where
    jump :: Int -> TokenStream (L tok) -> ([L tok], TokenStream (L tok))
    jump _ TsEof          = ([], TsEof)
    jump _ (TsError _)    = ([], TsEof)
    jump 0 (TsToken x xs) = ([], TsToken x xs)
    jump m (TsToken x xs) = let (ys, zs) = jump (m - 1) xs in (x:ys, zs)

takeWhile_' :: (L tok -> Bool) -> TokenStream (L tok) -> ([L tok], TokenStream (L tok))
takeWhile_' p stream = case take1_' stream of
  Nothing        -> ([], stream)
  Just (x, rest) ->
    if p x
      then let (xs, rest') = takeWhile_' p rest in (x:xs, rest')
      else ([], stream)

reachOffset' :: Int
             -> PosState (TokenStream (L tok))
             -> (String, PosState (TokenStream (L tok)))
reachOffset' n posState = case takeN_' (n - pstateOffset posState) (pstateInput posState) of
  -- the stream is empty
  Nothing      -> ("<empty line>", posState)
  Just ([], _) -> ("<empty line>", posState)

  Just (pre, post) -> ("<not yet implemented>", posState')
    where
      latestToken = last pre
      endingPos = case latestToken of
                    L NoLoc _ -> error "missing source location"
                    L (Loc _ end) _ -> end

      -- updated 'PosState'
      posState' = PosState
        { pstateInput = post
        , pstateOffset = max n (pstateOffset posState)
        , pstateSourcePos = toSourcePos endingPos
        , pstateTabWidth = pstateTabWidth posState
        , pstateLinePrefix = pstateLinePrefix posState
        }

--------------------------------------------------------------------------------
-- Helpers

toSourcePos :: Pos -> SourcePos
toSourcePos (Pos filename line column _) =
    SourcePos filename (mkPos line) (mkPos column)
