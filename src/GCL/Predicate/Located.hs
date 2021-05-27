module GCL.Predicate.Located where

import Data.Loc (Located, locOf, Loc (..))
import GCL.Predicate (Pred (..), Stmt (..), PO (..), Origin (..), Spec (..))


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

-- comparing only the constructor and the predicate

instance Located PO where
  locOf (PO _ _ _ o) = locOf o

instance Located Origin where
  locOf (AtAbort l) = l
  locOf (AtSkip l) = l
  locOf (AtSpec l) = l
  locOf (AtAssignment l) = l
  locOf (AtAssertion l) = l
  locOf (AtIf l) = l
  locOf (AtLoop l) = l
  locOf (AtTermination l) = l

instance Located Spec where
  locOf (Specification _ _ _ l) = l

instance Ord Loc where
  compare NoLoc NoLoc = EQ
  compare NoLoc (Loc _ _) = LT
  compare (Loc _ _) NoLoc = GT
  compare (Loc x _) (Loc y _) = compare x y