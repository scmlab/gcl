{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE TypeSynonymInstances #-}

module Syntax.Location where

import Data.Loc
import Data.Text.Lazy (Text)
import qualified Syntax.Abstract as A
import Syntax.Concrete
import qualified Syntax.Predicate as P
import Prelude hiding (Ordering (..))



--------------------------------------------------------------------------------
-- Relocatable

instance Relocatable Interval where
  reloc l (Interval x y _) = Interval x y l

instance Relocatable Type where
  reloc l (TBase base _) = TBase base l
  reloc l (TArray i s _) = TArray i s l
  reloc l (TFunc s t _) = TFunc s t l
  reloc l (TVar x _) = TVar x l

instance Relocatable Expr where
  reloc l (Var x _) = Var x l
  reloc l (Const x _) = Const x l
  reloc l (Lit x _) = Lit x l
  reloc l (App x y _) = App x y l
  reloc l (Lam x e _) = Lam x e l
  reloc l (Op x _) = Op x l
  reloc l (Hole _) = Hole l
  reloc l (Quant op xs r t _) = Quant op xs r t l
  reloc _ (Subst e s) = Subst e s

instance Relocatable Name where
  reloc l (Name x _) = Name x l

instance Relocatable P.Pred where
  reloc _ (P.Constant e) = P.Constant e
  reloc l (P.GuardIf e _) = P.GuardIf e l
  reloc l (P.GuardLoop e _) = P.GuardLoop e l
  reloc l (P.Assertion e _) = P.Assertion e l
  reloc l (P.LoopInvariant e b _) = P.LoopInvariant e b l
  reloc l (P.Bound e _) = P.Bound e l
  reloc _ (P.Conjunct ps) = P.Conjunct ps
  reloc _ (P.Disjunct ps) = P.Disjunct ps
  reloc _ (P.Negate p) = P.Negate p

--------------------------------------------------------------------------------
-- Remove location

class Located a => Departable a b | a -> b where
  depart :: a -> b

instance Departable Name Text where
  depart (Name x _) = x

instance Departable Interval A.Interval where
  depart (Interval a b _) = A.Interval (depart a) (depart b)

instance Departable Endpoint A.Endpoint where
  depart (Including e) = A.Including (depart e)
  depart (Excluding e) = A.Excluding (depart e)

instance Departable Type A.Type where
  depart (TBase base _) = A.TBase base
  depart (TArray i s _) = A.TArray (depart i) (depart s)
  depart (TFunc s t _) = A.TFunc (depart s) (depart t)
  depart (TVar x _) = A.TVar $ depart x

instance Departable Expr A.Expr where
  depart (Var x _) = A.Var $ depart x
  depart (Const x _) = A.Const $ depart x
  depart (Lit x _) = A.Lit x
  depart (App x y _) = A.App (depart x) (depart y)
  depart Lam {} = error "depart Lam to be implemented"
  depart (Op x _) = A.Op x
  depart (Hole _) = A.Hole 0 []
  depart (Quant op xs rng trm _) =
    A.Quant (depart op) (map depart xs) (depart rng) (depart trm)
  depart (Subst expr env) = A.Subst (depart expr) (fmap depart env)

--------------------------------------------------------------------------------
-- Add locations

class Located b => Hydratable a b | b -> a where
  hydrate :: a -> b

instance Hydratable Text Name where
  hydrate x = Name x NoLoc

instance Hydratable A.Interval Interval where
  hydrate (A.Interval a b) = Interval (hydrate a) (hydrate b) NoLoc

instance Hydratable A.Endpoint Endpoint where
  hydrate (A.Including e) = Including (hydrate e)
  hydrate (A.Excluding e) = Excluding (hydrate e)

instance Hydratable A.Type Type where
  hydrate (A.TBase base) = TBase base NoLoc
  hydrate (A.TArray i s) = TArray (hydrate i) (hydrate s) NoLoc
  hydrate (A.TFunc s t) = TFunc (hydrate s) (hydrate t) NoLoc
  hydrate (A.TVar x) = TVar (hydrate x) NoLoc

instance Hydratable A.Expr Expr where
  hydrate (A.Var x) = Var (hydrate x) NoLoc
  hydrate (A.Const x) = Const (hydrate x) NoLoc
  hydrate (A.Lit x) = Lit x NoLoc
  hydrate (A.App x y) = App (hydrate x) (hydrate y) NoLoc
  hydrate (A.Op x) = Op x NoLoc
  hydrate (A.Hole _ _) = Hole NoLoc
  hydrate (A.Quant op xs rng trm) =
    Quant (hydrate op) (map hydrate xs) (hydrate rng) (hydrate trm) NoLoc

--------------------------------------------------------------------------------
-- ToNoLoc

-- Like Relocatable, but recursively relocates everything to NoLoc
class ToNoLoc a where
  toNoLoc :: a -> a

instance ToNoLoc Name where
  toNoLoc (Name x _) = Name x NoLoc

instance ToNoLoc Interval where
  toNoLoc (Interval x y _) = Interval x y NoLoc

instance ToNoLoc Type where
  toNoLoc (TBase base _) = TBase base NoLoc
  toNoLoc (TArray i s _) = TArray (toNoLoc i) (toNoLoc s) NoLoc
  toNoLoc (TFunc s t _) = TFunc (toNoLoc s) (toNoLoc t) NoLoc
  toNoLoc (TVar x _) = TVar (toNoLoc x) NoLoc

instance ToNoLoc Expr where
  toNoLoc (Var x _) = Var (toNoLoc x) NoLoc
  toNoLoc (Const x _) = Const (toNoLoc x) NoLoc
  toNoLoc (Lit x _) = Lit x NoLoc
  toNoLoc (App x y _) = App (toNoLoc x) (toNoLoc y) NoLoc
  toNoLoc (Lam x e _) = Lam x (toNoLoc e) NoLoc
  toNoLoc (Op x _) = Op x NoLoc
  toNoLoc (Hole _) = Hole NoLoc
  toNoLoc (Quant op xs r t _) =
    Quant (toNoLoc op) (map toNoLoc xs) (toNoLoc r) (toNoLoc t) NoLoc
  toNoLoc (Subst e s) = Subst e s

instance ToNoLoc P.Pred where
  toNoLoc (P.Constant e) = P.Constant (toNoLoc e)
  toNoLoc (P.GuardIf e _) = P.GuardIf (toNoLoc e) NoLoc
  toNoLoc (P.GuardLoop e _) = P.GuardLoop (toNoLoc e) NoLoc
  toNoLoc (P.Assertion e _) = P.Assertion (toNoLoc e) NoLoc
  toNoLoc (P.LoopInvariant e b _) =
    P.LoopInvariant (toNoLoc e) (toNoLoc b) NoLoc
  toNoLoc (P.Bound e _) = P.Bound (toNoLoc e) NoLoc
  toNoLoc (P.Conjunct ps) = P.Conjunct (map toNoLoc ps)
  toNoLoc (P.Disjunct ps) = P.Disjunct (map toNoLoc ps)
  toNoLoc (P.Negate p) = P.Negate (toNoLoc p)

instance ToNoLoc a => ToNoLoc (L a) where
  toNoLoc (L _ a) = L NoLoc (toNoLoc a)

instance ToNoLoc P.PO where
  toNoLoc (P.PO i p q o) = P.PO i (toNoLoc p) (toNoLoc q) (toNoLoc o)

instance ToNoLoc P.Origin where
  toNoLoc (P.AtAbort _) = P.AtAbort NoLoc
  toNoLoc (P.AtSkip _) = P.AtSkip NoLoc
  toNoLoc (P.AtSpec _) = P.AtSpec NoLoc
  toNoLoc (P.AtAssignment _) = P.AtAssignment NoLoc
  toNoLoc (P.AtAssertion _) = P.AtAssertion NoLoc
  toNoLoc (P.AtIf _) = P.AtIf NoLoc
  toNoLoc (P.AtLoop _) = P.AtLoop NoLoc
  toNoLoc (P.AtTermination _) = P.AtTermination NoLoc

instance ToNoLoc P.Spec where
  toNoLoc (P.Specification i p q _) =
    P.Specification i (toNoLoc p) (toNoLoc q) NoLoc
