{-# LANGUAGE OverloadedStrings #-}

module Pretty.Predicate where

import Data.Text.Prettyprint.Doc
import Prelude hiding (Ordering(..))

import Syntax.Predicate
import Pretty.Concrete ()
import Pretty.Variadic

--------------------------------------------------------------------------------
-- | Pred

instance PrettyPrec Pred where
  prettyPrec n = prettyPrec n . toExpr

instance Pretty Pred where
  pretty = prettyPrec 0
