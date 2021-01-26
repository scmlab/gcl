{-# LANGUAGE TypeSynonymInstances #-}

module Syntax.Location where

-- import Data.Loc
-- import Data.Text.Lazy (Text)
-- import Syntax.Concrete
-- import Prelude hiding (Ordering (..))



--------------------------------------------------------------------------------
-- Remove location

-- class Located a => Departable a b | a -> b where
--   depart :: a -> b

-- instance Departable Name Text where
--   depart (Name x _) = x

-- instance Departable Expr Expr where
--   depart (Var x _) = Var $ depart x
--   depart (Const x _) = Const $ depart x
--   depart (Lit x _) = Lit x
--   depart (App x y _) = App (depart x) (depart y)
--   depart Lam {} = error "depart Lam to be implemented"
--   depart (Op x _) = Op x
--   depart (Hole _) = Hole 0 []
--   depart (Quant op xs rng trm _) =
--     Quant (depart op) (map depart xs) (depart rng) (depart trm)
--   depart (Subst expr env) = Subst (depart expr) (fmap depart env)

--------------------------------------------------------------------------------
-- Add locations

-- class Located b => Hydratable a b | b -> a where
--   hydrate :: a -> b

-- instance Hydratable Text Name where
--   hydrate x = Name x NoLoc

-- instance Hydratable Expr Expr where
--   hydrate (Var x) = Var (hydrate x) NoLoc
--   hydrate (Const x) = Const (hydrate x) NoLoc
--   hydrate (Lit x) = Lit x NoLoc
--   hydrate (App x y) = App (hydrate x) (hydrate y) NoLoc
--   hydrate (Op x) = Op x NoLoc
--   hydrate (Hole _ _) = Hole NoLoc
--   hydrate (Quant op xs rng trm) =
--     Quant (hydrate op) (map hydrate xs) (hydrate rng) (hydrate trm) NoLoc

-- 