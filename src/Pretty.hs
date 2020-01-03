{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE TypeSynonymInstances, FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Pretty where

import Data.Text.Prettyprint.Doc
import Control.Monad ((>=>))
import Control.Monad.Free
import Prelude hiding (Ordering(..))

import Syntax.Abstract (Expr(..), Lit(..), Op(..), classify)
import Syntax.Concrete (Fixity(..))
import Syntax.Lasagne
import GCL.PreCond (Obligation(..), Specification(..))

--------------------------------------------------------------------------------
-- | Expr

data VarArg a = Expect (Expr -> VarArg a) | Complete a
  deriving (Functor)

instance Applicative VarArg where
  pure = Complete
  Complete f  <*> Complete x = Complete (f x)
  Expect   f  <*> Complete x = Expect (\arg -> f arg  <*> pure x)
  Complete f  <*> Expect   g = Expect (\arg -> pure f <*> g arg)
  Expect   f  <*> Expect   g = Expect (\arg -> f arg  <*> g arg)

instance Monad VarArg where
  return = Complete
  Complete x >>= f = f x
  Expect   g >>= f = Expect (g >=> f)

parensIf :: Int -> Int -> Doc ann -> Doc ann
parensIf n m
  | n > m     = parens
  | otherwise = id

var :: VarArg Expr
var = Expect Complete

handleOp :: Int -> Op -> VarArg (Doc ann)
handleOp n op = case classify op of
  Infix m -> do
    p <- var
    q <- var
    return $ parensIf n m $
      prettyPrec (succ m) p
      <+> pretty op
      <+> prettyPrec (succ m) q
  InfixL m -> do
    p <- var
    q <- var
    return $ parensIf n m $
      prettyPrec m p
      <+> pretty op
      <+> prettyPrec (succ m) q
  InfixR m -> do
    p <- var
    q <- var
    return $ parensIf n m $
      prettyPrec (succ m) p
      <+> pretty op
      <+> prettyPrec m q
  Prefix m -> do
    p <- var
    return $ parensIf n m $
      pretty op
      <+> prettyPrec m p
  Postfix m -> do
    p <- var
    return $ parensIf n m $
      prettyPrec m p
      <+> pretty op

handleExpr :: Int -> Expr -> VarArg (Doc ann)
handleExpr _ (Var v) = return $ pretty v
handleExpr _ (Const c) = return $ pretty c
handleExpr _ (Lit lit) = return $ pretty lit
handleExpr n (Op op) = handleOp n op
handleExpr n (App p q) = case handleExpr n p of
  Expect f -> f q
  Complete s -> do
    t <- handleExpr n q
    -- see if the second argument is an application, apply parenthesis when needed
    case q of
      App _ _ -> return $ s <+> parensIf n 0 t
      _ ->       return $ s <+> t


handleExpr _ (Hole i _) = return $ "[" <> pretty i <> "]"

prettyPrec :: Int -> Expr -> Doc ann
prettyPrec n expr = case handleExpr n expr of
  Expect   _ -> mempty
  Complete s -> s

instance Pretty Expr where
  pretty = prettyPrec 0

instance Pretty Lit where
  pretty (Num i) = pretty $ show i
  pretty (Bol b) = pretty $ show b
  pretty (Chr c) = pretty $ show c


instance Pretty Op where
  pretty EQ = "="
  pretty NEQ = "≠"
  pretty LTE = "≤"
  pretty GTE = "≥"
  pretty LT = "<"
  pretty GT = ">"
  pretty Implies = "→"
  pretty Conj = "→"
  pretty Disj = "⋀"
  pretty Neg = "¬"
  pretty Add = "+"
  pretty Sub = "-"
  pretty Mul = "*"
  pretty Div = "/"

--------------------------------------------------------------------------------
-- | Obligation & Specification

instance Pretty Obligation where
  pretty (Obligation i p q) = lbracket <> pretty i <> rbracket <+> line <>
    indent 2 (pretty p) <> line <>
    indent 2 (pretty q) <> line

instance Pretty Specification where
  pretty (Specification i hardness p q _) = lbracket <> pretty i <> rbracket <+> pretty (show hardness) <> line <>
    indent 2 (pretty p) <> line <>
    indent 2 (pretty q) <> line

--------------------------------------------------------------------------------
-- | Lasagne

instance Pretty (GdCmd Program) where
  pretty (GdCmd p x) = "|" <+> pretty p <+> "->" <> line <> indent 2 (pretty x) <> line

instance Pretty (Stmt Program) where
  pretty (Skip x) = "skip" <> line <> pretty x
  pretty (Abort x) = "abort" <> line <> pretty x
  pretty (Assign vars exprs x)
    = hcat (punctuate comma (map pretty vars))
    <+> ":="
    <+> hcat (punctuate comma (map pretty exprs))
    <> line <> pretty x
  pretty (Assert p x)
    = lbrace <+> pretty p <+> rbrace
    <> line <> pretty x
  pretty (Do inv bnd branches x)
    = lbrace <+> pretty inv <+> comma <+> "bnd: " <> pretty bnd <+> rbrace <> line
    <> "  do" <> line
    <> indent 2 (vsep (map pretty branches)) <> line
    <> "  od" <> line
    <> pretty x
  pretty (If Nothing branches x)
    = "  if" <> line
    <> indent 2 (vsep (map pretty branches)) <> line
    <> "  fi" <> line
    <> pretty x
  pretty (If (Just p) branches x)
    = lbrace <+> pretty p <+> rbrace <> line
    <> "  if" <> line
    <> indent 2 (vsep (map pretty branches)) <> line
    <> "  fi" <> line
    <> pretty x
  pretty (Spec x)
    = "  {!" <> line
    <> " !}" <> line
    <> pretty x

instance Pretty Program where
  pretty (Pure p) = line <> "# " <> pretty p
  pretty (Free (Sauce p _ x)) = line <> "# " <> pretty p <> line <> line <> "  " <> (pretty x)
