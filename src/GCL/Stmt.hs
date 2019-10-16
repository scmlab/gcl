module GCL.Stmt where

import GCL.Expr
import GCL.Pred
import GCL.EnumHole

data Stmt = Skip
          | Assign [VName] [Expr]
          | Seq Stmt Stmt
          | Assert Pred
          | If Pred [GdCmd]
          | Do Pred Expr [GdCmd]
  deriving (Show, Eq)

-- Guarded Command (was Branch)

data GdCmd = GdCmd Pred Stmt deriving (Show, Eq)

unGdCmd (GdCmd guard stmt) = (guard, stmt)

instance EnumHole Stmt where
  enumHole Skip = return Skip
  enumHole (Assign xs es) =
    Assign xs <$> mapM enumHole es
  enumHole (Seq c1 c2) =
    Seq <$> enumHole c1 <*> enumHole c2
  enumHole (Assert p) =
    Assert <$> enumHole p
  enumHole (If pre branches) =
    If pre <$> mapM enumHole branches
  enumHole (Do inv bnd branches) =
    Do  <$> enumHole inv
        <*> enumHole bnd
        <*> mapM enumHole branches

instance EnumHole GdCmd where
  enumHole (GdCmd guard body) = GdCmd <$> enumHole guard <*> enumHole body
