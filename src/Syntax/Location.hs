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

-- 