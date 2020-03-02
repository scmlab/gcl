{-# LANGUAGE MultiParamTypeClasses, TypeSynonymInstances,
      FunctionalDependencies#-}

module Syntax.Location where

import Prelude hiding (Ordering(..))

import Data.Text.Lazy (Text)
import Data.Loc

import qualified Syntax.Abstract as A
import qualified Syntax.Predicate as P
import Syntax.Concrete

instance Located Program where
  locOf (Program _ _ l) = l

instance Located Stmt where
  locOf (Skip              l) = l
  locOf (Abort             l) = l
  locOf (Assign _ _        l) = l
  locOf (Assert _          l) = l
  locOf (LoopInvariant _ _ l) = l
  locOf (Do _              l) = l
  locOf (If _              l) = l
  locOf (SpecQM            l) = l
  locOf (Spec              l) = l
--
-- instance Located Expr where
--   locOf (Var _ l)   = l
--   locOf (Const _ l) = l
--   locOf (Lit _ l)   = l
--   locOf (Op _ l)    = l
--   locOf (App _ _ l) = l
--   locOf (Hole l)    = l
--   locOf (Quant _ _ _ _ l) = l

instance Located Type where
  locOf (TBase _    l) = l
  locOf (TArray _ _ l) = l
  locOf (TFunc _ _  l) = l
  locOf (TVar _     l) = l

instance Located Interval where
  locOf (Interval _ _ l) = l

instance Located Endpoint where
  locOf (Including e) = locOf e
  locOf (Excluding e) = locOf e

instance Located Lit where
  locOf _ = NoLoc

instance Located Lower where
  locOf (Lower _ l) = l

instance Located Upper where
  locOf (Upper _ l) = l

--------------------------------------------------------------------------------
-- Relocatable

instance Relocatable Interval where
  reloc l (Interval x y _) = Interval x y l

instance Relocatable Type where
  reloc l (TBase  base _) = TBase  base l
  reloc l (TArray i s _)  = TArray i s l
  reloc l (TFunc  s t _)  = TFunc  s t l
  reloc l (TVar   x _)    = TVar   x l

instance Relocatable Expr where
  reloc l (Var x    _) = Var x l
  reloc l (Const x  _) = Const x l
  reloc l (Lit x    _) = Lit x l
  reloc l (App x y  _) = App x y l
  reloc l (Op  x    _) = Op x l
  reloc l (Hole     _) = Hole l
  reloc l (Quant op xs r t _) = Quant op xs r t l

instance Relocatable Upper where
  reloc l (Upper x _) = Upper x l

instance Relocatable Lower where
  reloc l (Lower x _) = Lower x l

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

instance Departable Upper Text where
  depart (Upper x _) = x

instance Departable Lower Text where
  depart (Lower x _) = x

instance Departable Interval A.Interval where
  depart (Interval a b _) = A.Interval (depart a) (depart b)

instance Departable Endpoint A.Endpoint where
  depart (Including e) = A.Including (depart e)
  depart (Excluding e) = A.Excluding (depart e)

instance Departable Type A.Type where
  depart (TBase  base _) = A.TBase  base
  depart (TArray i s _)  = A.TArray (depart i) (depart s)
  depart (TFunc  s t _)  = A.TFunc  (depart s) (depart t)
  depart (TVar   x _)    = A.TVar   $ depart x

instance Departable Expr A.Expr where
  depart (Var x    _) = A.Var   $ depart x
  depart (Const x  _) = A.Const $ depart x
  depart (Lit x    _) = A.Lit   x
  depart (App x y  _) = A.App   (depart x) (depart y)
  depart (Op  x    _) = A.Op    x
  depart (Hole     _) = A.Hole  0 []
  depart (Quant op xs rng trm _) =
    A.Quant (depart op) (map depart xs) (depart rng) (depart trm)

--------------------------------------------------------------------------------
-- Add locations

class Located b => Hydratable a b | b -> a where
  hydrate :: a -> b

instance Hydratable Text Upper where
  hydrate x = Upper x NoLoc

instance Hydratable Text Lower where
  hydrate x = Lower x NoLoc

instance Hydratable A.Interval Interval where
  hydrate (A.Interval a b) = Interval (hydrate a) (hydrate b) NoLoc

instance Hydratable A.Endpoint Endpoint where
  hydrate (A.Including e) = Including (hydrate e)
  hydrate (A.Excluding e) = Excluding (hydrate e)

instance Hydratable A.Type Type where
  hydrate (A.TBase  base) = TBase  base NoLoc
  hydrate (A.TArray i s ) = TArray (hydrate i) (hydrate s) NoLoc
  hydrate (A.TFunc  s t ) = TFunc  (hydrate s) (hydrate t) NoLoc
  hydrate (A.TVar   x   ) = TVar   (hydrate x) NoLoc

instance Hydratable A.Expr Expr where
  hydrate (A.Var x  ) = Var   (hydrate x) NoLoc
  hydrate (A.Const x) = Const (hydrate x) NoLoc
  hydrate (A.Lit x  ) = Lit   x NoLoc
  hydrate (A.App x y) = App   (hydrate x) (hydrate y) NoLoc
  hydrate (A.Op  x  ) = Op    x NoLoc
  hydrate (A.Hole _ _) = Hole NoLoc
  hydrate (A.Quant op xs rng trm) =
    Quant (hydrate op) (map hydrate xs) (hydrate rng) (hydrate trm) NoLoc

--------------------------------------------------------------------------------
-- ToNoLoc

-- Like Relocatable, but recursively relocates everything to NoLoc
class ToNoLoc a where
  toNoLoc :: a -> a

instance ToNoLoc Upper where
  toNoLoc (Upper x _) = Upper x NoLoc

instance ToNoLoc Lower where
  toNoLoc (Lower x _) = Lower x NoLoc

instance ToNoLoc Interval where
  toNoLoc (Interval x y _) = Interval x y NoLoc

instance ToNoLoc Type where
  toNoLoc (TBase  base _) = TBase  base NoLoc
  toNoLoc (TArray i s _)  = TArray (toNoLoc i) (toNoLoc s) NoLoc
  toNoLoc (TFunc  s t _)  = TFunc  (toNoLoc s) (toNoLoc t) NoLoc
  toNoLoc (TVar   x _)    = TVar   (toNoLoc x) NoLoc

instance ToNoLoc Expr where
  toNoLoc (Var x    _) = Var (toNoLoc x) NoLoc
  toNoLoc (Const x  _) = Const (toNoLoc x) NoLoc
  toNoLoc (Lit x    _) = Lit x NoLoc
  toNoLoc (App x y  _) = App (toNoLoc x) (toNoLoc y) NoLoc
  toNoLoc (Op  x    _) = Op x NoLoc
  toNoLoc (Hole     _) = Hole NoLoc
  toNoLoc (Quant op xs r t _) = Quant (toNoLoc op) (map (toNoLoc) xs) (toNoLoc r) (toNoLoc t) NoLoc

instance ToNoLoc P.Pred where
  toNoLoc (P.Constant e) = P.Constant (toNoLoc e)
  toNoLoc (P.GuardIf e _) = P.GuardIf (toNoLoc e) NoLoc
  toNoLoc (P.GuardLoop e _) = P.GuardLoop (toNoLoc e) NoLoc
  toNoLoc (P.Assertion e _) = P.Assertion (toNoLoc e) NoLoc
  toNoLoc (P.LoopInvariant e b _) = P.LoopInvariant (toNoLoc e) (toNoLoc b) NoLoc
  toNoLoc (P.Bound e _) = P.Bound (toNoLoc e) NoLoc
  toNoLoc (P.Conjunct ps) = P.Conjunct (map (toNoLoc) ps)
  toNoLoc (P.Disjunct ps) = P.Disjunct (map (toNoLoc) ps)
  toNoLoc (P.Negate p) = P.Negate (toNoLoc p)

instance ToNoLoc a => ToNoLoc (L a) where
  toNoLoc (L _ a) = L NoLoc (toNoLoc a)
