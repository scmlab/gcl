{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE DefaultSignatures #-}

module Syntax.Parser.TokenStream
  ( PrettyToken(..)
  ) where

import           Data.List.NonEmpty             ( NonEmpty(..)
                                                , nonEmpty
                                                )
import qualified Data.List.NonEmpty            as NE
import           Data.Loc
import           Data.Proxy
import           Language.Lexer.Applicative
import           Text.Megaparsec         hiding ( Pos )


-- | How to print tokens
class PrettyToken tok where
  prettyTokens :: NonEmpty (L tok) -> String
  default prettyTokens :: Show tok => NonEmpty (L tok) -> String
  prettyTokens = init . unlines . map snd . showTokenLines

  -- convert it back to its original string representation (inverse of lexing)
  -- defaulted to `show`
  restoreToken :: tok -> String
  default restoreToken :: Show tok => tok -> String
  restoreToken = show


instance Ord tok => Ord (TokenStream (L tok)) where
  compare _ _ = EQ

instance Ord tok => Stream (TokenStream (L tok)) where
  type Token (TokenStream (L tok)) = L tok
  type Tokens (TokenStream (L tok)) = [L tok]
  tokenToChunk Proxy tok = [tok]
  tokensToChunk Proxy = id
  chunkToTokens Proxy = id
  chunkLength Proxy = chunkLength'
  chunkEmpty Proxy = chunkEmpty'
  take1_     = take1_'
  takeN_     = takeN_'
  takeWhile_ = takeWhile_'

instance (Ord tok, PrettyToken tok) => VisualStream (TokenStream (L tok)) where
  showTokens Proxy = prettyTokens

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
takeN_' n s | n <= 0        = Just ([], s)
            | streamEmpty s = Just ([], s)
            | otherwise     = Just (jump n s)
 where
  jump :: Int -> TokenStream (L tok) -> ([L tok], TokenStream (L tok))
  jump _ TsEof          = ([], TsEof)
  jump _ (TsError _   ) = ([], TsEof)
  jump 0 (TsToken x xs) = ([], TsToken x xs)
  jump m (TsToken x xs) = let (ys, zs) = jump (m - 1) xs in (x : ys, zs)

takeWhile_'
  :: (L tok -> Bool) -> TokenStream (L tok) -> ([L tok], TokenStream (L tok))
takeWhile_' p stream = case take1_' stream of
  Nothing        -> ([], stream)
  Just (x, rest) -> if p x
    then let (xs, rest') = takeWhile_' p rest in (x : xs, rest')
    else ([], stream)

--------------------------------------------------------------------------------
-- Helpers

-- returns lines + line numbers of the string representation of tokens
showTokenLines :: PrettyToken tok => NonEmpty (L tok) -> [(Int, String)]
showTokenLines (x :| xs) = zip lineNumbers (NE.toList body)
 where
  glued :: Chunk
  glued              = foldl glue (toChunk x) (map toChunk xs)

  Chunk start body _ = glued

  lineNumbers :: [Int]
  lineNumbers = [fst start + 1 ..]

data Chunk = Chunk (Int, Int)        -- ^ start, counting from 0
                              (NonEmpty String) -- ^ payload
                                                (Int, Int)        -- ^ end, counting from 0
  deriving Show


toChunk :: PrettyToken tok => L tok -> Chunk
toChunk (L NoLoc tok) = Chunk start strings end
 where
  strings = case nonEmpty (lines (restoreToken tok)) of
    Nothing -> "" :| []
    Just xs -> xs
  start = (0, 0)
  end   = (NE.length strings - 1, length (NE.last strings))

toChunk (L (Loc from to) tok) = Chunk start strings end
 where
  strings = case nonEmpty (lines (restoreToken tok)) of
    Nothing -> "" :| []
    Just xs -> xs
  start = (posLine from - 1, posCol from - 1)
  end   = (posLine to - 1, posCol to)

glue :: Chunk -> Chunk -> Chunk
glue (Chunk start1 body1 end1) (Chunk start2 body2 end2) = Chunk start1
                                                                 body
                                                                 end2
 where
  lineGap :: Int
  lineGap = fst start2 - fst end1

  body :: NonEmpty String
  body = case lineGap of

    -- two chunk meet at the same line
    0 ->
      let (line :| body2') = body2
          colGap           = snd start2 - snd end1
          line'            = NE.last body1 ++ replicate colGap ' ' ++ line
      in  case nonEmpty (NE.init body1) of
            Nothing     -> line' :| body2'
            Just body1' -> body1' <> (line' :| body2')

    -- two chunk differs by one line
    1 ->
      let (line :| body2') = body2
          colGap           = snd start2
          line'            = replicate colGap ' ' ++ line
      in  body1 <> (line' :| body2')

    -- two chunk differs by more than one line
    n ->
      let (line :| body2') = body2
          colGap           = snd start2
          line'            = replicate colGap ' ' ++ line
          emptyLines       = NE.fromList (replicate (n - 1) "")
      in  body1 <> emptyLines <> (line' :| body2')
