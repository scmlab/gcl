{-# LANGUAGE TypeSynonymInstances, FlexibleInstances #-}

module GCL.Stmt where

import Control.Monad (liftM2, liftM3)

import GCL.Expr
import GCL.Pred
import GCL.EnumHole

type OIdx = Int
-- newtype OIdx = OIdx {unOIdx :: Int}
--    deriving Show
type Branch = (Pred, Stmt)
data Stmt = Skip
          | Assign [VName] [Expr]
          | Seq Stmt Stmt
          | Assert Pred
          | If [Branch]
          | Do Pred Expr [Branch]
  deriving Show

instance EnumHole Stmt where
  enumHole Skip = return Skip
  enumHole (Assign xs es) =
    Assign xs <$> mapM enumHole es
  enumHole (Seq c1 c2) =
    liftM2 Seq (enumHole c1) (enumHole c2)
  enumHole (Assert p) =
    Assert <$> enumHole p
  enumHole (If branches) =
    If <$> mapM enumHole branches
  enumHole (Do inv bnd branches) =
    liftM3 Do (enumHole inv)
              (enumHole bnd)
              (mapM enumHole branches)

instance EnumHole Branch where
  enumHole (guard, body) = liftM2 (,) (enumHole guard) (enumHole body)
