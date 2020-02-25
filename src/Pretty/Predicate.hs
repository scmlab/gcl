{-# LANGUAGE OverloadedStrings #-}

module Pretty.Predicate where

import Data.Text.Prettyprint.Doc
import Prelude hiding (Ordering(..))

import Syntax.Predicate
import Pretty.Concrete ()
import Pretty.Util

--------------------------------------------------------------------------------
-- | Pred

instance PrettyPrec Pred where
  prettyPrec n predicate = case predicate of
    Constant p -> "Constant" <+> prettyPrec n p
    Guard p (IF _) _ -> "Guard IF" <+> prettyPrec n p
    Guard p (LOOP _) _ -> "Guard LOOP" <+> prettyPrec n p
    Assertion p _ -> "Assertion" <+> prettyPrec n p
    LoopInvariant p b _ -> "LoopInvariant" <+> prettyPrec n p <+> "bnd:" <+> prettyPrec n b
    Bound p _ -> "Bound" <+> prettyPrec n p
    Conjunct ps -> "Conjunct" <+> prettyList ps
    Disjunct ps -> "Disjunct" <+> prettyList ps
    Negate p -> "Negate" <+> prettyPrec n p

instance Pretty Pred where
  pretty = prettyPrec 0
