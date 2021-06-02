module Syntax.Common.Located where

import Data.Loc
import Syntax.Common
import Prelude hiding (Ordering (..))

instance (Located a, Located b) => Located(Either a b) where
  locOf (Left a) = locOf a
  locOf (Right b) = locOf b

instance Located ChainOp where
  locOf (EQProp l) = l
  locOf (EQPropU l) = l
  locOf (EQ l) = l
  locOf (NEQ l) = l
  locOf (NEQU l) = l
  locOf (LTE l) = l
  locOf (LTEU l) = l
  locOf (GTE l) = l
  locOf (GTEU l) = l
  locOf (LT l) = l
  locOf (GT l) = l

instance Located ArithOp where
  locOf (Implies l) = l
  locOf (ImpliesU l) = l
  locOf (Disj l) = l
  locOf (DisjU l) = l
  locOf (Conj l) = l
  locOf (ConjU l) = l
  locOf (Neg l) = l
  locOf (NegU l) = l
  locOf (Add l) = l
  locOf (Sub l) = l
  locOf (Mul l) = l
  locOf (Div l) = l
  locOf (Mod l) = l
  locOf (Max l) = l
  locOf (Min l) = l

instance Located QuantOp where
  locOf (Sum l) = l
  locOf (Exists l) = l
  locOf (Forall l) = l
  locOf (Hash l) = l

instance Located Op where
  locOf (ChainOp op) = locOf op
  locOf (ArithOp op) = locOf op
  locOf (QuantOp op) = locOf op