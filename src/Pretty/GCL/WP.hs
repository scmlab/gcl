module Pretty.GCL.WP where

import Data.Text.Prettyprint.Doc
import Prelude hiding (Ordering(..))

import GCL.WP

import Pretty.Abstract.Simple ()

--------------------------------------------------------------------------------
-- | Obligation & Specification

instance Pretty Obligation where
  pretty (Obligation i p q) = lbracket <> pretty i <> rbracket <+> line <>
    indent 2 (pretty p) <> line <>
    indent 2 (pretty q) <> line

instance Pretty Specification where
  pretty (Specification i hardness p q _) = lbracket <> pretty i <> rbracket <+> pretty (show hardness) <> line <>
    indent 2 (pretty p) <> line <>
    indent 2 (pretty q) <> line
