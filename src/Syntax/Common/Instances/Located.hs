module Syntax.Common.Instances.Located where

import           Data.Loc
import           Syntax.Common.Types
import           Prelude                 hiding ( Ordering(..) )

instance (Located a, Located b) => Located(Either a b) where
  locOf (Left  a) = locOf a
  locOf (Right b) = locOf b

instance Located Name where
  locOf (Name _ l) = l

instance Located ChainOp where
  locOf (EQProp  l) = l
  locOf (EQPropU l) = l
  locOf (EQ      l) = l
  locOf (NEQ     l) = l
  locOf (NEQU    l) = l
  locOf (LTE     l) = l
  locOf (LTEU    l) = l
  locOf (GTE     l) = l
  locOf (GTEU    l) = l
  locOf (LT      l) = l
  locOf (GT      l) = l

instance Located ArithOp where
  locOf (Implies  l) = l
  locOf (ImpliesU l) = l
  locOf (Disj     l) = l
  locOf (DisjU    l) = l
  locOf (Conj     l) = l
  locOf (ConjU    l) = l
  locOf (Neg      l) = l
  locOf (NegU     l) = l
  locOf (NegNum   l) = l
  locOf (Add      l) = l
  locOf (Sub      l) = l
  locOf (Mul      l) = l
  locOf (Div      l) = l
  locOf (Mod      l) = l
  locOf (Max      l) = l
  locOf (Min      l) = l
  locOf (Exp      l) = l
  locOf (Hash     l) = l
  locOf (PointsTo l) = l
  locOf (SConj    l) = l
  locOf (SImp     l) = l

instance Located TypeOp where
  locOf (Arrow l) = l
instance Located Op where
  locOf (ChainOp op) = locOf op
  locOf (ArithOp op) = locOf op
  locOf (TypeOp op) = locOf op