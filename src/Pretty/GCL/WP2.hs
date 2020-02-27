{-# LANGUAGE OverloadedStrings #-}

module Pretty.GCL.WP2 where

import Data.Text.Prettyprint.Doc
import Prelude hiding (Ordering(..))
import Data.Loc ()

import GCL.WP2

import Pretty.GCL.WP ()
import Pretty.Abstract ()
import Pretty.Concrete ()
import Pretty.Predicate ()

--------------------------------------------------------------------------------
-- | ObliOrigin

instance Pretty Origin where

  pretty (AtAbort          l) = "Abort" <+> pretty l
  pretty (AtSkip           l) = "Skip" <+> pretty l
  pretty (AtSpec           l) = "Spec" <+> pretty l
  pretty (AtAssignment     l) = "Assigment" <+> pretty l
  pretty (AtAssertion      l) = "Assertion" <+> pretty l
  pretty (AtLoopInvariant  l) = "LoopInvariant" <+> pretty l
  pretty (AtIf             l) = "If" <+> pretty l
  pretty (AtLoop           l) = "Loop" <+> pretty l
  pretty (AtTermination    l) = "Termination" <+> pretty l
  pretty (AtBoundDecrement l) = "BoundDecrement" <+> pretty l

--------------------------------------------------------------------------------
-- | Obligation & Specification

instance Pretty PO where
  pretty (PO i p q os) = lbracket <> pretty i <> rbracket <+> line <>
    indent 2 (pretty p) <> line <>
    indent 2 (pretty q) <> line <>
    indent 2 (pretty os) <> line

instance Pretty Specification2 where
  pretty (Specification i p q _) = lbracket <> pretty i <> rbracket <> line <>
    indent 2 (pretty p) <> line <>
    indent 2 (pretty q) <> line

--------------------------------------------------------------------------------
-- | StructError

instance Pretty StructError2 where
  pretty (MissingLoopInvariant loc) = "Missing Loop Invariant" <+> pretty loc
  pretty (MissingBound loc) = "Missing Bound" <+> pretty loc
  pretty (MissingPrecondition loc) = "Missing Precondition" <+> pretty loc
  pretty (MissingPostcondition loc) = "Missing Postcondition" <+> pretty loc
  pretty (DigHole loc) = "Dig Hole" <+> pretty loc
