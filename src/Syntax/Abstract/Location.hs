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

instance Located Expr where
  locOf (Var _ l)   = l
  locOf (Const _ l) = l
  locOf (Lit _ l)   = l
  locOf (Op _ l)    = l
  locOf (App _ _ l) = l
  locOf (Quant _ _ _ _ l) = l

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

instance Located Base where
  locOf _ = NoLoc

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
  reloc l (Quant op xs r t _) = Quant op xs r t l

--------------------------------------------------------------------------------
-- Remove location

class Located a => Departable a b | a -> b where
  depart :: a -> b

instance Departable Lit A.Lit where
  depart (Num x) = A.Num x
  depart (Bol x) = A.Bol x
  depart (Chr x) = A.Chr x

instance Departable Upper Text where
  depart (Upper x _) = x

instance Departable Lower Text where
  depart (Lower x _) = x

instance Departable Base A.TBase where
  depart TInt  = A.TInt
  depart TBool = A.TBool
  depart TChar = A.TChar

instance Departable Interval A.Interval where
  depart (Interval a b _) = A.Interval (depart a) (depart b)

instance Departable Endpoint A.Endpoint where
  depart (Including e) = A.Including (depart e)
  depart (Excluding e) = A.Excluding (depart e)

instance Departable Type A.Type where
  depart (TBase  base _) = A.TBase  $ depart base
  depart (TArray i s _)  = A.TArray (depart i) (depart s)
  depart (TFunc  s t _)  = A.TFunc  (depart s) (depart t)
  depart (TVar   x _)    = A.TVar   $ depart x

instance Departable Expr A.Expr where
  depart (Var x    _) = A.Var   $ depart x
  depart (Const x  _) = A.Const $ depart x
  depart (Lit x    _) = A.Lit   $ depart x
  depart (App x y  _) = A.App   (depart x) (depart y)
  depart (Op  x    _) = A.Op     x
  depart (Quant op xs rng trm _) =
    A.Quant (depart op) (map depart xs) (depart rng) (depart trm)
--
-- instance Departable Op A.Op where
--   depart (EQ  _) = A.EQ
--   depart (NEQ  _) = A.NEQ
--   depart (LTE _) = A.LTE
--   depart (GTE _) = A.GTE
--   depart (LT  _) = A.LT
--   depart (GT  _) = A.GT
--   depart (Implies _) = A.Implies
--   depart (Conj  _) = A.Conj
--   depart (Disj  _) = A.Disj
--   depart (Neg   _) = A.Neg
--   depart (Add   _) = A.Add
--   depart (Sub   _) = A.Sub
--   depart (Mul   _) = A.Mul
--   depart (Div   _) = A.Div
--   depart (Mod   _) = A.Mod
