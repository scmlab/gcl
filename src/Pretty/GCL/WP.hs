{-# LANGUAGE OverloadedStrings #-}

module Pretty.GCL.WP where

import Data.Text.Prettyprint.Doc
import Prelude hiding (Ordering(..))
import Data.Loc

import GCL.WP

import Pretty.Abstract ()
import Pretty.Concrete ()
import Pretty.Variadic

--------------------------------------------------------------------------------
-- | Pred

instance PrettyPrec Pred where
  prettyPrec n = prettyPrec n . predToExpr

instance Pretty Pred where
  pretty = prettyPrec 0


--------------------------------------------------------------------------------
-- | ObliOrigin

instance Pretty ObliOrigin where
  pretty (AroundAbort l) = "AroundAbort" <+> pretty l
  pretty (AroundSkip l) = "AroundSkip" <+> pretty l
  pretty (AssertGuaranteed l) = "AssertGuaranteed" <+> pretty l
  pretty (AssertSufficient l) = "AssertSufficient" <+> pretty l
  pretty (Assignment l) = "Assignment" <+> pretty l
  pretty (IfTotal l) = "IfTotal" <+> pretty l
  pretty (IfBranch l) = "IfBranch" <+> pretty l
  pretty (LoopBase l) = "LoopBase" <+> pretty l
  pretty (LoopInd l) = "LoopInd" <+> pretty l
  pretty (LoopTermBase l) = "LoopTermBase" <+> pretty l
  pretty (LoopTermDec l) = "LoopTermDec" <+> pretty l
  pretty (LoopInitialize l) = "LoopInitialize" <+> pretty l

--------------------------------------------------------------------------------
-- | Obligation & Specification

instance Pretty Obligation where
  pretty (Obligation i p q os) = lbracket <> pretty i <> rbracket <+> line <>
    indent 2 (pretty p) <> line <>
    indent 2 (pretty q) <> line <>
    indent 2 (pretty os) <> line
  -- pretty (ObliIfTotal p qs l) = "ObliIfTotal" <> line <>
  --   indent 2 (pretty p) <> line <>
  --   indent 2 (pretty qs) <> line <>
  --   indent 2 (pretty l) <> line

instance Pretty Specification where
  pretty (Specification i p q _) = lbracket <> pretty i <> rbracket <> line <>
    indent 2 (pretty p) <> line <>
    indent 2 (pretty q) <> line

--------------------------------------------------------------------------------
-- | Misc

instance Pretty Loc where
  pretty = pretty . displayLoc
