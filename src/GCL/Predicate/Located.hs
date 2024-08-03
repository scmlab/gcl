module GCL.Predicate.Located where

import Data.Loc (Located, locOf, Loc (..))
import GCL.Predicate (Pred (..))

{-
instance Located Pred where
  locOf (Constant _) = NoLoc
  locOf (GuardIf _ l) = l
  locOf (GuardLoop _ l) = l
  locOf (Assertion _ l) = l
  locOf (LoopInvariant _ _ l) = l
  locOf (Bound _ l) = l
  locOf (Conjunct _) = NoLoc
  locOf (Disjunct _) = NoLoc
  locOf (Negate _) = NoLoc

instance Located Stmt where
  locOf (Skip l) = locOf l
  locOf (Abort l) = locOf l
  locOf (Assign l _ _) = locOf l
  locOf (Do l _ _) = locOf l
  locOf (If l _) = locOf l
  locOf (Spec l _) = locOf l
-}

-- instance Ord Loc where
--   compare NoLoc NoLoc = EQ
--   compare NoLoc (Loc _ _) = LT
--   compare (Loc _ _) NoLoc = GT
--   compare (Loc x _) (Loc y _) = compare x y
