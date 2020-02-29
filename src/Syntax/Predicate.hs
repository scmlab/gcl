{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

module Syntax.Predicate where

import Data.Aeson
import Data.Text.Lazy (Text)
import qualified Data.Text.Lazy as Text
import Data.Loc
import Data.Char (isUpper)
import GHC.Generics

import qualified Syntax.Concrete as C
import Syntax.Concrete (Expr, Lower, Fresh, Subst)

--------------------------------------------------------------------------------
-- | Predicates

data Pred = Constant  Expr
          | GuardIf   Expr Loc
          | GuardLoop Expr Loc
          | Assertion Expr Loc
          | LoopInvariant Expr Expr Loc -- predicate & bound
          | Bound     Expr Loc
          | Conjunct  [Pred]
          | Disjunct  [Pred]
          | Negate     Pred
          deriving (Eq, Show, Generic)

instance ToJSON Pred where

toExpr :: Pred -> Expr
toExpr (Constant e) = e
toExpr (Bound e _) = e
toExpr (Assertion e _) = e
toExpr (LoopInvariant e _ _) = e
toExpr (GuardIf e _) = e
toExpr (GuardLoop e _) = e
toExpr (Conjunct xs) = C.conjunct (map toExpr xs)
toExpr (Disjunct xs) = C.disjunct (map toExpr xs)
toExpr (Negate x) = C.neg (toExpr x)

subst :: Fresh m => Subst -> Pred -> m Pred
subst env (Constant e) = Constant <$> C.subst env e
subst env (Bound e l) = Bound <$> C.subst env e <*> pure l
subst env (Assertion e l) = Assertion <$> C.subst env e <*> pure l
subst env (LoopInvariant e b l) = LoopInvariant <$> C.subst env e <*> pure b <*> pure l
subst env (GuardIf e l) = GuardLoop <$> C.subst env e <*> pure l
subst env (GuardLoop e l) = GuardLoop <$> C.subst env e <*> pure l
subst env (Conjunct xs) = Conjunct <$> mapM (subst env) xs
subst env (Disjunct es) = Disjunct <$> mapM (subst env) es
subst env (Negate x) = Negate <$> subst env x

--------------------------------------------------------------------------------
-- | Smart constructors for testing

assertion :: Expr -> Pred
assertion x = Assertion x NoLoc

loopInvariant :: Expr -> Text -> Pred
loopInvariant x b = LoopInvariant x (bnd b) NoLoc
  where bnd = if Text.null b
                then C.variable
                else if isUpper (Text.head b)
                  then C.constant
                  else C.variable

guardIf :: Expr -> Pred
guardIf x = GuardIf x NoLoc

guardLoop :: Expr -> Pred
guardLoop x = GuardLoop x NoLoc

boundEq :: Expr -> Expr -> Pred
boundEq x var = Bound (x `C.eqq` var) NoLoc

boundLT :: Expr -> Expr -> Pred
boundLT x var = Bound (x `C.lt` var) NoLoc

boundGTE :: Expr -> Expr -> Pred
boundGTE x var = Bound (x `C.gte` var) NoLoc


(===) :: Int -> Int -> Expr
x === y = C.number x `C.eqq` C.number y

conjunct :: [Pred] -> Pred
conjunct [] = Constant C.true
conjunct [x] = x
conjunct xs = Conjunct xs

disjunct :: [Pred] -> Pred
disjunct [] = Constant C.false
disjunct [x] = x
disjunct xs = Disjunct xs

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

data Struct = Struct
                Pred    --  assertion
                [Stmt]  --  statements after the assertion
                Struct  --  the next chunk of assertion with statements after it
            | Postcond Pred
            deriving (Eq)

-- extracting the assertion from a Struct
extractAssertion :: Struct -> Pred
extractAssertion (Struct p _ _) = p
extractAssertion (Postcond p)   = p

--------------------------------------------------------------------------------
-- | Statement with its computed precondition

data Stmt
  = Skip   (L Pred)
  | Abort  (L Pred)
  | Assign (L Pred) [Lower] [Expr]
  | Do     (L Pred) Expr [GdCmd]
  | If     (L Pred)      [GdCmd]
  | Spec   (L Pred) Pred -- pre and post

data GdCmd = GdCmd
  { gdCmdGuard :: Pred
  , gdCmdBody :: Struct
  }
  deriving (Eq)

-- comparing only the constructor and the predicate
instance Eq Stmt where
  Skip l        == Skip m       = l == m
  Abort l       == Abort m      = l == m
  Assign l _ _  == Assign m _ _ = l == m
  Do l _ xs     == Do m _ ys    = l == m && xs == ys
  If l xs       == If m ys      = l == m && xs == ys
  Spec l _      == Spec m _     = l == m
  _             == _            = False

precond :: Stmt -> Pred
precond (Skip   l    ) = unLoc l
precond (Abort  l    ) = unLoc l
precond (Assign l _ _) = unLoc l
precond (Do     l _ _) = unLoc l
precond (If     l _  ) = unLoc l
precond (Spec   l _  ) = unLoc l
