{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE RecordWildCards #-}


module Syntax.Test where

import Text.Megaparsec
import Data.Proxy
import Data.Loc

-- import Data.List.NonEmpty (NonEmpty (..))
import Language.Lexer.Applicative

instance Ord tok => Ord (TokenStream (L tok)) where
  compare _ _ = EQ

instance Ord tok => Stream (TokenStream (L tok)) where
  type Token (TokenStream (L tok)) = L tok
  type Tokens (TokenStream (L tok)) = [L tok]
  tokenToChunk Proxy = tokenToChunk'
  tokensToChunk Proxy = tokensToChunk'
  chunkToTokens Proxy = chunkToTokens'
  chunkLength Proxy = chunkLength'
  chunkEmpty Proxy = chunkEmpty'
  take1_ = take1_'
  takeN_ = takeN_'
  -- takeWhile_ = takeWhile_'
  -- showTokens Proxy = stringPretty
  --
  -- reachOffset o pst =
  --   reachOffset' splitAt foldl' show tokToChar (TokNewline, TokTab) o pst
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
chunkLength' (x:[]) = tokenEndingOffset x
chunkLength' (_:xs) = chunkLength' xs

chunkEmpty' :: [L tok] -> Bool
chunkEmpty' = (==) 0 . chunkLength'

streamEmpty :: TokenStream (L tok) -> Bool
streamEmpty TsEof                     = True
streamEmpty (TsError _)               = True
streamEmpty (TsToken tok TsEof)       = tokenEndingOffset tok == 0
streamEmpty (TsToken tok (TsError _)) = tokenEndingOffset tok == 0
streamEmpty (TsToken tok rest)        =
    if tokenEndingOffset tok == 0
      then streamEmpty rest
      else False

take1_' :: TokenStream (L tok) -> Maybe (L tok, TokenStream (L tok))
take1_' (TsToken tok rest) = Just (tok, rest)
take1_' _                  = Nothing

takeN_' :: Int -> TokenStream (L tok) -> Maybe ([L tok], TokenStream (L tok))
takeN_' n s
  | n <= 0        = Just ([], s)
  | streamEmpty s = Nothing
  | otherwise     =
      let len = nextOffset s in
      if n < len then Just ([], s)      -- not enough to cover the first token
                 else case take1_' s of
                        Nothing        -> error "impossible"
                        Just (x, rest) -> case takeN_' (n - len) rest of
                          Just (xs, rest') -> Just (x:xs, rest')
                          Nothing          -> Just ([x], TsEof)

-- takeWhile_' :: (L tok -> Bool) -> TokenStream (L tok) -> (TokenStream (L tok), TokenStream (L tok))

--------------------------------------------------------------------------------
-- Helpers


-- | Get the offset of the ending position of a Loc
locOffset :: Loc -> Int
locOffset NoLoc = 0
locOffset (Loc _ end) = posCoff end

tokenEndingOffset :: L tok -> Int
tokenEndingOffset (L loc _) = locOffset loc

-- | Get the offset of the next token
nextOffset :: TokenStream (L tok) -> Int
nextOffset (TsToken tok _) = tokenEndingOffset tok
nextOffset _               = 0

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
--   -> (Token s, Token s)
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
--              $ takeWhile_ (/= newlineTok) post
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
--       in if | ch == newlineTok ->
--                 St (SourcePos n (l <> pos1) pos1)
--                    id
--             | ch == tabTok ->
--                 St (SourcePos n l (mkPos $ c' + w - ((c' - 1) `rem` w)))
--                    (g . (fromTok ch :))
--             | otherwise ->
--                 St (SourcePos n l (c <> pos1))
--                    (g . (fromTok ch :))
-- {-# INLINE reachOffset' #-}
--
--
-- -- | @stringPretty s@ returns pretty representation of string @s@. This is
-- -- used when printing string tokens in error messages.
--
-- stringPretty :: NonEmpty Tok -> String
-- stringPretty = show
--
-- -- | Replace tab characters with given number of spaces.
--
-- expandTab
--   :: Pos
--   -> String
--   -> String
-- expandTab w' = go 0
--   where
--     go 0 []        = []
--     go 0 ('\t':xs) = go w xs
--     go 0 (x:xs)    = x : go 0 xs
--     go n xs        = ' ' : go (n - 1) xs
--     w              = unPos w'
