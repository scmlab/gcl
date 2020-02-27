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
import Syntax.Concrete (Expr, Fresh, Subst)

--------------------------------------------------------------------------------
-- | Predicates

data Sort = IF Loc | LOOP Loc
          deriving (Eq, Show, Generic)

data Pred = Constant  Expr
          | GuardIf   Expr Loc
          | GuardLoop Expr Loc
          | Assertion Expr Loc
          | LoopInvariant Expr Expr Loc -- predicate & bound
          | Bound     Expr Loc
          | Conjunct  [Pred]
          | Disjunct  [Pred]
          | Negate     Pred
          -- | Imply      Pred  Pred
          deriving (Eq, Show, Generic)

instance ToJSON Sort where
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
