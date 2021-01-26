{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE TypeSynonymInstances #-}

module Syntax.Location where

import Data.Loc
import Data.Text.Lazy (Text)
import qualified Syntax.Abstract as A
import Syntax.Concrete
import Prelude hiding (Ordering (..))



--------------------------------------------------------------------------------
-- Remove location

class Located a => Departable a b | a -> b where
  depart :: a -> b

instance Departable Name Text where
  depart (Name x _) = x

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