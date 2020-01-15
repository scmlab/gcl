{-# LANGUAGE MultiParamTypeClasses, TypeSynonymInstances,
      FunctionalDependencies#-}

module Syntax.Abstract.Location where

import Prelude hiding (Ordering(..))

import Data.Text.Lazy (Text)
import Data.Loc

import qualified Syntax.Abstract as A
import Syntax.Concrete

instance Located Stmt where
  locOf (Skip              l) = l
  locOf (Abort             l) = l
  locOf (Assign _ _        l) = l
  locOf (Assert _          l) = l
  locOf (AssertWithBnd _ _ l) = l
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
