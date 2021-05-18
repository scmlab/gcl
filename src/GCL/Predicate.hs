{-# LANGUAGE DeriveGeneric #-}

module GCL.Predicate where

import Data.Loc (Loc(..), L)
import Syntax.Abstract (Expr)
import GHC.Generics (Generic)
import Data.Aeson (ToJSON)
import Syntax.Common (Name)

-- | Predicates
data Pred
  = Constant Expr
  | GuardIf Expr Loc
  | GuardLoop Expr Loc
  | Assertion Expr Loc
  | LoopInvariant Expr Expr Loc -- predicate & bound
  | Bound Expr Loc
  | Conjunct [Pred]
  | Disjunct [Pred]
  | Negate Pred
  deriving (Eq, Show, Generic)

instance ToJSON Pred

--------------------------------------------------------------------------------

-- | Data structure for storing Assertions in a program
--
--    { P }   ----- Struct { P } ----
--    stmt
--    stmt
--    stmt
--    { Q }    ----- Struct { Q } ----
--    stmt
--    stmt
--    stmt
--    { R }    ----- Postcond { R } ----
data Struct
  = Struct
      Pred --  assertion
      [Stmt] --  statements after the assertion
      Struct --  the next chunk of assertion with statements after it
  | Postcond Pred
  deriving (Eq)


--------------------------------------------------------------------------------

-- | Statement with its computed precondition
data Stmt
  = Skip (L Pred)
  | Abort (L Pred)
  | Assign (L Pred) [Name] [Expr]
  | Do (L Pred) Expr [GdCmd]
  | If (L Pred) [GdCmd]
  | Spec (L Pred) Pred -- pre and post

data GdCmd = GdCmd Pred Struct
  deriving (Eq)

instance Eq Stmt where
  Skip l == Skip m = l == m
  Abort l == Abort m = l == m
  Assign l _ _ == Assign m _ _ = l == m
  Do l _ xs == Do m _ ys = l == m && xs == ys
  If l xs == If m ys = l == m && xs == ys
  Spec l _ == Spec m _ = l == m
  _ == _ = False

--------------------------------------------------------------------------------

-- | Obligation
data PO
  = PO Int Pred Pred Origin
  deriving (Eq, Show, Generic)

data Origin
  = AtAbort Loc
  | AtSkip Loc
  | AtSpec Loc
  | AtAssignment Loc
  | AtAssertion Loc -- AssertSufficient
  | AtIf Loc
  | AtLoop Loc
  | AtTermination Loc
  deriving (Eq, Show, Generic)

data Spec = Specification
  { specID :: Int,
    specPreCond :: Pred,
    specPostCond :: Pred,
    specLoc :: Loc
  }
  deriving (Eq, Show, Generic)
