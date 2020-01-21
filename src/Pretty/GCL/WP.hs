module Pretty.GCL.WP where

import Data.Text.Prettyprint.Doc
import Prelude hiding (Ordering(..))

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
-- | Obligation & Specification

instance Pretty Obligation where
  pretty (Obligation i p q os) = lbracket <> pretty i <> rbracket <+> line <>
    indent 2 (pretty p) <> line <>
    indent 2 (pretty q) <> line <>
    indent 2 (pretty (show os)) <> line

instance Pretty Specification where
  pretty (Specification i p q _) = lbracket <> pretty i <> rbracket <> line <>
    indent 2 (pretty p) <> line <>
    indent 2 (pretty q) <> line
