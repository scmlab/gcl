{-# LANGUAGE OverloadedStrings #-}

module Pretty.GCL.WP2 where

import Data.Text.Prettyprint.Doc
import Prelude hiding (Ordering(..))
import Data.Loc

import GCL.WP2

import Pretty.GCL.WP ()
import Pretty.Abstract ()
import Pretty.Concrete ()
import Pretty.Predicate ()

--------------------------------------------------------------------------------
-- | ObliOrigin

instance Pretty ObliOrigin2 where
  pretty (AroundAbort l) = "AroundAbort" <+> pretty l
  pretty (AroundSkip l) = "AroundSkip" <+> pretty l
  pretty (AssertGuaranteed l) = "AssertGuaranteed" <+> pretty l
  pretty (AssertSufficient l) = "AssertSufficient" <+> pretty l
  pretty (Assignment l) = "Assignment" <+> pretty l
  pretty (IfTotal l) = "IfTotal" <+> pretty l
  pretty (LoopBase l) = "LoopBase" <+> pretty l
  pretty (LoopTermBase l) = "LoopTermBase" <+> pretty l
  pretty (LoopInitialize l) = "LoopInitialize" <+> pretty l

--------------------------------------------------------------------------------
-- | Obligation & Specification

instance Pretty Obligation2 where
  pretty (Obligation i p q os) = lbracket <> pretty i <> rbracket <+> line <>
    indent 2 (pretty p) <> line <>
    indent 2 (pretty q) <> line <>
    indent 2 (pretty os) <> line

instance Pretty Specification2 where
  pretty (Specification i p q _) = lbracket <> pretty i <> rbracket <> line <>
    indent 2 (pretty p) <> line <>
    indent 2 (pretty q) <> line
