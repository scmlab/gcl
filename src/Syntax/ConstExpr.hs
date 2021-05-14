{-# LANGUAGE OverloadedStrings #-}

module Syntax.ConstExpr where

import Data.List (partition)
import qualified Data.Map as Map
import Data.Maybe (mapMaybe)
import Syntax.Abstract
import Syntax.Abstract.Util ( extractAssertion, extractLetBinding )
import Syntax.Common

constExpr :: [Name] -> Expr -> Bool
constExpr ns (Paren expr) = constExpr ns expr
constExpr _ (Lit _ _) = True
constExpr bvars (Var v _) = v `elem` bvars
constExpr _ (Const _ _) = True
constExpr _ (Op _) = True
constExpr bvars (Chain a _ b _) = constExpr bvars a && constExpr bvars b
constExpr bvars (App e1 e2 _) = constExpr bvars e1 && constExpr bvars e2
constExpr bvars (Lam x e _) = constExpr (x : bvars) e
constExpr _ (Hole _) = True --- is this right?
constExpr bvars (Quant op bvs range body _) =
    constExprQOp op
    && constExpr (bvs ++ bvars) range
    && constExpr (bvs ++ bvars) body
    where
      constExprQOp qop = case qop of
          Left _ -> True
          Right expr -> constExpr bvars expr
constExpr _ (Subst _ _) = error "constExpr Subst to be implemented"

-- extract assertions from declarations
pickGlobals :: [Declaration] -> ([Expr], [Expr])
pickGlobals = partition (constExpr []) . mapMaybe extractAssertion

-- extract let bindings in declarations
pickLetBindings :: [Declaration] -> Defns
pickLetBindings = Map.fromList . mapMaybe extractLetBinding
