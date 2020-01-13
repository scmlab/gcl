{-# LANGUAGE MultiParamTypeClasses, TypeSynonymInstances,
      FunctionalDependencies#-}

module Syntax.Abstract.Location where

import Prelude hiding (Ordering(..))

import Data.Loc

import qualified Syntax.Concrete as C
import Syntax.Abstract

instance Located C.Stmt where
  locOf (C.Skip              l) = l
  locOf (C.Abort             l) = l
  locOf (C.Assign _ _        l) = l
  locOf (C.Assert _          l) = l
  locOf (C.AssertWithBnd _ _ l) = l
  locOf (C.Do _              l) = l
  locOf (C.If _              l) = l
  locOf (C.SpecQM            l) = l
  locOf (C.Spec              l) = l

instance Located C.Op where
  locOf (C.EQ l)      = l
  locOf (C.NEQ l)     = l
  locOf (C.LTE l)     = l
  locOf (C.GTE l)     = l
  locOf (C.LT l)      = l
  locOf (C.GT l)      = l
  locOf (C.Implies l) = l
  locOf (C.Conj l)    = l
  locOf (C.Disj l)    = l
  locOf (C.Neg l)     = l
  locOf (C.Add l)     = l
  locOf (C.Sub l)     = l
  locOf (C.Mul l)     = l
  locOf (C.Div l)     = l
  locOf (C.Mod l)     = l

instance Located C.Expr where
  locOf (C.Var _ l)   = l
  locOf (C.Const _ l) = l
  locOf (C.Lit _ l)   = l
  locOf (C.Op _ l)    = l
  locOf (C.App _ _ l) = l
  locOf (C.Hole l)    = l

instance Located C.Type where
  locOf (C.TBase _    l) = l
  locOf (C.TArray _ _ l) = l
  locOf (C.TFunc _ _  l) = l
  locOf (C.TVar _     l) = l

instance Located C.Interval where
  locOf (C.Interval _ _ l) = l

instance Located C.Endpoint where
  locOf (C.Including e) = locOf e
  locOf (C.Excluding e) = locOf e

instance Located C.Base where
  locOf _ = NoLoc

instance Located C.Lit where
  locOf _ = NoLoc

instance Located C.Lower where
  locOf (C.Lower _ l) = l

instance Located C.Upper where
  locOf (C.Upper _ l) = l

--------------------------------------------------------------------------------
-- Remove location

{-
type DepartM = ExceptT ConvertError (State Index)

abstract :: Departable a b => a -> Either ConvertError b
abstract = runAbstractM . depart

runAbstractM :: AbstractM a -> Either ConvertError a
runAbstractM f = evalState (runExceptT f) 0

index :: AbstractM Index
index = do i <- get
           put (succ i)
           return i
-}

class Located a => Departable a b | a -> b where
  depart :: a -> b

instance Departable C.Lit Lit where
  depart (C.Num x) = Num x
  depart (C.Bol x) = Bol x

instance Departable C.Upper Const where
  depart (C.Upper x _) = x

instance Departable C.Lower Var where
  depart (C.Lower x _) = x

instance Departable C.Base TBase where
  depart C.TInt  = TInt
  depart C.TBool = TBool
  depart C.TChar = TChar

instance Departable C.Interval Interval where
  depart (C.Interval a b _) = Interval (depart a) (depart b)

instance Departable C.Endpoint Endpoint where
  depart (C.Including e) = Including (depart e)
  depart (C.Excluding e) = Excluding (depart e)

instance Departable C.Type Type where
  depart (C.TBase  base _) = TBase  $ depart base
  depart (C.TArray i s _)  = TArray (depart i) (depart s)
  depart (C.TFunc  s t _)  = TFunc  (depart s) (depart t)
  depart (C.TVar   x _)    = TVar   $ depart x

instance Departable C.Expr Expr where
  depart (C.Var x    _) = Var   $ depart x
  depart (C.Const x  _) = Const $ depart x
  depart (C.Lit x    _) = Lit   $ depart x
  depart (C.App x y  _) = App   (depart x) (depart y)
  depart (C.Op  x    _) = Op    $ depart x
  depart (C.Hole     _) = undefined -- Hole  index []
     -- SCM: it's the only case where we need a monad.
     -- Can this be avoided, or done elsewhere?

instance Departable C.Op Op where
  depart (C.EQ  _) = EQ
  depart (C.NEQ  _) = NEQ
  depart (C.LTE _) = LTE
  depart (C.GTE _) = GTE
  depart (C.LT  _) = LT
  depart (C.GT  _) = GT
  depart (C.Implies _) = Implies
  depart (C.Conj  _) = Conj
  depart (C.Disj  _) = Disj
  depart (C.Neg   _) = Neg
  depart (C.Add   _) = Add
  depart (C.Sub   _) = Sub
  depart (C.Mul   _) = Mul
  depart (C.Div   _) = Div
  depart (C.Mod   _) = Mod
