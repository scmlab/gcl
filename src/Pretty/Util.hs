{-# LANGUAGE OverloadedStrings #-}

module Pretty.Util where

import Data.Text.Prettyprint.Doc
import Prelude hiding (Ordering(..))

--------------------------------------------------------------------------------
-- | List

-- prettyList2 :: Pretty a => [a] -> Doc ann
-- prettyList2 [] = "[]"
-- prettyList2 xs = indent 2 $ list $ map pretty xs
--  -- "[" <+> prettyList2' xs <+> "]"
--
--   -- where
--   --   prettyList2' [] = mempty
--   --   prettyList2' [x] = pretty x
--   --   prettyList2' (x:xs) = pretty x <> line <> "," <+> prettyList2' xs
--
-- prettyPrecList :: PrettyPrec a => Int -> [a] -> Doc ann
-- prettyPrecList n [] = "[]"
-- prettyPrecList n xs = "[" <+> prettyPrecList' n xs <+> "]"
--
--   where
--     prettyPrecList' n [] = mempty
--     prettyPrecList' n [x] = prettyPrec n x
--     prettyPrecList' n (x:xs) = prettyPrec n x <> line <> "," <+> prettyPrecList' n xs

--------------------------------------------------------------------------------
-- | Pretty print with Precedence

class PrettyPrec a where
  prettyPrec :: Int -> a -> Doc ann
