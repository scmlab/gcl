{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE RecordWildCards #-}


module Syntax.Test where

import Text.Megaparsec hiding (Pos)
import qualified Text.Megaparsec as Mega
import Data.Proxy
import Data.Loc
import Data.Char (isSpace)

import Data.List.NonEmpty (NonEmpty (..))
import Language.Lexer.Applicative

import Syntax.LexerTest
import Debug.Trace

instance Streamable Tok where
  showNonEmptyTokens (L _ x :| xs) = showToken x ++ concat (map (showToken . unLoc) xs)


class Ord tok => Streamable tok where
  showNonEmptyTokens :: NonEmpty (L tok) -> String
  -- newlineToken :: Maybe tok
  -- tabToken :: Maybe tok


instance Ord tok => Ord (TokenStream (L tok)) where
  compare _ _ = EQ

instance Streamable tok => Stream (TokenStream (L tok)) where
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
  -- reachOffset o pst =
  --   reachOffset'
  --     splitAt
  --     foldl'
  --     fromToks
  --     fromTok
  --     (newlineToken, tabToken)
  --     o
  --     pst


  -- reachOffsetNoLine o pst =
  --   reachOffsetNoLine' splitAt foldl' ('\n', '\t') o pst

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
    go target TsEof                   = Nothing
    go target (TsError _)             = Nothing
    go target (TsToken (L NoLoc _) _) = error "token missing source location"
    go target (TsToken x rest) =
      let next = tokenEndingOffset x in
      traceShow ("next token end", next) $
      traceShow ("taking", target) $
      if target < next
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
reachOffset' n posState =
-- reachOffset' n posState@(PosState input offset sourcePos tabWidth linePrefix) =
  case take1_' (pstateInput posState) of
    -- the stream is empty
    Nothing -> ("<empty line>", posState)
    -- the stream is kaput
    Just (L NoLoc _, _) -> ("<missing source location>", posState)
    Just (L (Loc _ end) _tok, input') ->
      if n > width
        then (resultLine, resultPosState)
        else ("<not yet implemented>", posState)
      where
        (resultLine, resultPosState) = reachOffset' (n - width) posState'

        width = posCoff end

        -- updated 'PosState'
        posState' = PosState
            { pstateInput = input'
            , pstateOffset = pstateOffset posState + width
            , pstateSourcePos = toSourcePos end
            , pstateTabWidth = pstateTabWidth posState
            , pstateLinePrefix = pstateLinePrefix posState
            }

        -- currentRow = posLine end
        -- nextRow    = posLine end



  -- case takeN_' n input of
  --   Nothing -> ("<empty line>", posState)
  --   Just (xs, input')

--------------------------------------------------------------------------------
-- Helpers

-- | Get the offset of the ending position of a Loc
-- locOffset :: Loc -> Int
-- locOffset NoLoc = 0
-- locOffset (Loc _ end) = posCoff end + 1

tokenWidth :: L tok -> Int
tokenWidth (L NoLoc _) = 0
tokenWidth (L (Loc start end) _) = posCoff end - posCoff start + 1

tokenEndingOffset :: L tok -> Int
tokenEndingOffset (L NoLoc _) = 0
tokenEndingOffset (L (Loc _ end) _) = posCoff end + 1

-- | Get the "width" of the next token
nextTokenWidth :: TokenStream (L tok) -> Int
nextTokenWidth (TsToken tok _) = tokenWidth tok
nextTokenWidth _               = 0

-- -- | Get the ending offset of the next token
-- nextTokenEndingOffset :: TokenStream (L tok) -> Int
-- nextTokenEndingOffset (TsToken (L (Loc _ end) _) _) = posCoff end + 1
-- nextTokenEndingOffset _               = 0

toSourcePos :: Pos -> SourcePos
toSourcePos (Pos filename line column _) =
    SourcePos filename (mkPos line) (mkPos column)

-- ----------------------------------------------------------------------------
-- -- Helpers
--
-- -- | An internal helper state type combining a difference 'String' and an
-- -- unboxed 'SourcePos'.
--
-- data St = St SourcePos ShowS
--
-- -- | A helper definition to facilitate defining 'reachOffset' for various
-- -- stream types.
--
-- reachOffset'
--   :: forall s. Stream s
--   => (Int -> s -> (Tokens s, s))
--      -- ^ How to split input stream at given offset
--   -> (forall b. (b -> Token s -> b) -> b -> Tokens s -> b)
--      -- ^ How to fold over input stream
--   -> (Tokens s -> String)
--      -- ^ How to convert chunk of input stream into a 'String'
--   -> (Token s -> Char)
--      -- ^ How to convert a token into a 'Char'
--   -> (Maybe (Token s), Maybe (Token s))
--      -- ^ Newline token and tab token
--   -> Int
--      -- ^ Offset to reach
--   -> PosState s
--      -- ^ Initial 'PosState' to use
--   -> (String, PosState s)
--      -- ^ Line at which 'SourcePos' is located, updated 'PosState'
-- reachOffset' splitAt'
--              foldl''
--              fromToks
--              fromTok
--              (newlineTok, tabTok)
--              o
--              PosState {..} = (locatedLine' , posState)
--   where
--     -- Line at which 'SourcePos' is located
--     locatedLine = expandTab pstateTabWidth
--              . addPrefix
--              . f
--              . fromToks
--              . fst
--              $ takeWhile_ (\c -> Just c /= newlineTok) post
--     locatedLine' = case locatedLine of
--       "" -> "<empty line>"
--       xs -> xs
--
--     -- Updated 'PosState'
--     posState = PosState
--         { pstateInput = post
--         , pstateOffset = max pstateOffset o
--         , pstateSourcePos = spos
--         , pstateTabWidth = pstateTabWidth
--         , pstateLinePrefix =
--             if sameLine
--               then pstateLinePrefix ++ f ""
--               else f ""
--         }
--
--     addPrefix xs =
--       if sameLine
--         then pstateLinePrefix ++ xs
--         else xs
--     sameLine = sourceLine spos == sourceLine pstateSourcePos
--     (pre, post) = splitAt' (o - pstateOffset) pstateInput
--     St spos f = foldl'' go (St pstateSourcePos id) pre
--     go (St apos g) ch =
--       let SourcePos n l c = apos
--           c' = unPos c
--           w  = unPos pstateTabWidth
--       in if | Just ch == newlineTok ->
--                 St (SourcePos n (l <> pos1) pos1)
--                    id
--             | Just ch == tabTok ->
--                 St (SourcePos n l (mkPos $ c' + w - ((c' - 1) `rem` w)))
--                    (g . (fromTok ch :))
--             | otherwise ->
--                 St (SourcePos n l (c <> pos1))
--                    (g . (fromTok ch :))
-- {-# INLINE reachOffset' #-}
--
--
-- -- | Replace tab characters with given number of spaces.
--
-- expandTab
--   :: Mega.Pos
--   -> String
--   -> String
-- expandTab w' = go 0
--   where
--     go 0 []        = []
--     go 0 ('\t':xs) = go w xs
--     go 0 (x:xs)    = x : go 0 xs
--     go n xs        = ' ' : go (n - 1) xs
--     w              = unPos w'
