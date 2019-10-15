module GCL.Stmt where

import GCL.Expr
import GCL.Pred
import GCL.EnumHole

data Branch = Branch Pred Stmt deriving (Show)
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
    Seq <$> enumHole c1 <*> enumHole c2
  enumHole (Assert p) =
    Assert <$> enumHole p
  enumHole (If branches) =
    If <$> mapM enumHole branches
  enumHole (Do inv bnd branches) =
    Do  <$> enumHole inv
        <*> enumHole bnd
        <*> mapM enumHole branches

instance EnumHole Branch where
  enumHole (Branch guard body) = Branch <$> enumHole guard <*> enumHole body
