{-# LANGUAGE OverloadedStrings #-}

module Syntax.ConstExpr where

import           Data.List                      ( partition )
import           Data.Text.Lazy                 ( Text )

import           Syntax.Concrete         hiding ( unary
                                                , binary
                                                )
import           Syntax.Location                ( )
import           Data.Maybe                     ( mapMaybe )

constExpr :: [Text] -> Expr -> Bool
constExpr _     (Lit   _ _  ) = True
constExpr bvars (Var   v _  ) = lowerToText v `elem` bvars
constExpr _     (Const _ _  ) = True
constExpr _     (Op    _ _  ) = True
constExpr bvars (App e1 e2 _) = constExpr bvars e1 && constExpr bvars e2
constExpr _     (Hole _     ) = True  --- is this right?
constExpr bvars (Quant op bvs range body _) =
  constExpr bvars op
    && constExpr (bvs' ++ bvars) range
    && constExpr (bvs' ++ bvars) body
  where bvs' = map lowerToText bvs

pickGlobals :: [Expr] -> ([Expr], [Expr])
pickGlobals = partition (constExpr [])

-- extract assertions in declarations
extractAssertions :: [Declaration] -> [Expr]
extractAssertions = mapMaybe extractAssertion
