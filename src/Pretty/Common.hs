{-# LANGUAGE OverloadedStrings #-}

module Pretty.Common where

import Data.Text.Prettyprint.Doc (Pretty (pretty))
import Pretty.Util
import Syntax.Common
import Prelude hiding (Ordering (..))


-- | Name
-- instance Pretty Name where
--   pretty = toDoc . prettyWithLoc
instance PrettyWithLoc Name where
  prettyWithLoc (Name n l) = fromDoc l (pretty n)

-- | Operators
instance Pretty Op where
  pretty = toDoc . prettyWithLoc

instance PrettyWithLoc Op where
  prettyWithLoc (EQ l) = fromDoc l "="
  prettyWithLoc (NEQ l) = fromDoc l "/="
  prettyWithLoc (NEQU l) = fromDoc l "≠"
  prettyWithLoc (LTE l) = fromDoc l "<="
  prettyWithLoc (LTEU l) = fromDoc l "≤"
  prettyWithLoc (GTE l) = fromDoc l ">="
  prettyWithLoc (GTEU l) = fromDoc l "≥"
  prettyWithLoc (LT l) = fromDoc l "<"
  prettyWithLoc (GT l) = fromDoc l ">"
  prettyWithLoc (Implies l) = fromDoc l "=>"
  prettyWithLoc (ImpliesU l) = fromDoc l "→"
  prettyWithLoc (Conj l) = fromDoc l "&&"
  prettyWithLoc (ConjU l) = fromDoc l "∧"
  prettyWithLoc (Disj l) = fromDoc l "||"
  prettyWithLoc (DisjU l) = fromDoc l "∨"
  prettyWithLoc (Neg l) = fromDoc l "~"
  prettyWithLoc (NegU l) = fromDoc l "¬"
  prettyWithLoc (Add l) = fromDoc l "+"
  prettyWithLoc (Sub l) = fromDoc l "-"
  prettyWithLoc (Mul l) = fromDoc l "*"
  prettyWithLoc (Div l) = fromDoc l "/"
  prettyWithLoc (Mod l) = fromDoc l "%"
  prettyWithLoc (Sum l) = fromDoc l "Σ"
  prettyWithLoc (Forall l) = fromDoc l "∀"
  prettyWithLoc (Exists l) = fromDoc l "∃"
