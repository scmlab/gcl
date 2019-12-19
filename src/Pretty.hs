{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeSynonymInstances, FlexibleInstances #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts #-}

module Pretty where

import Data.Text.Prettyprint.Doc
import Control.Monad.Free
import Prelude hiding (Ordering(..))

import Syntax.Abstract (Expr(..), Lit(..), Op(..))
import Syntax.Lasagne


--------------------------------------------------------------------------------
-- | Expr

data Variadic doc = Expect (Expr -> Variadic doc) | Complete doc
data Fixity = Infix Int | InfixR Int | InfixL Int | Prefix Int | Postfix Int
  deriving (Show, Eq)

classify :: Op -> Fixity
classify Implies = InfixR 1
classify Disj = InfixL 2
classify Conj = InfixL 3
classify Neg = Prefix 4
classify EQ = Infix 5
classify LTE = Infix 5
classify GTE = Infix 5
classify LT = Infix 5
classify GT = Infix 5
classify Mul = InfixL 1
classify Div = InfixL 1
classify Plus = InfixL 2
classify Minus = InfixL 2

parensIf :: Int -> Int -> Doc ann -> Doc ann
parensIf n m
  | n > m     = parens
  | otherwise = id

handleOp :: Int -> Op -> Variadic (Doc ann)
handleOp n op = case classify op of
  Infix m -> Expect $ \p -> Expect $ \q -> Complete $ parensIf n m $
    prettyPrec (succ m) p
    <+> pretty op
    <+> prettyPrec (succ m) q
  InfixL m -> Expect $ \p -> Expect $ \q -> Complete $ parensIf n m $
    prettyPrec m p
    <+> pretty op
    <+> prettyPrec (succ m) q
  InfixR m -> Expect $ \p -> Expect $ \q -> Complete $ parensIf n m $
    prettyPrec (succ m) p
    <+> pretty op
    <+> prettyPrec m q
  Prefix m -> Expect $ \p -> Complete $ parensIf n m $
    pretty op
    <+> prettyPrec m p
  Postfix m -> Expect $ \p -> Complete $ parensIf n m $
    prettyPrec m p
    <+> pretty op

handleExpr :: Int -> Expr -> Variadic (Doc ann)
handleExpr _ (Var var) = Complete $ pretty var
handleExpr _ (Const con) = Complete $ pretty con
handleExpr _ (Lit lit) = Complete $ pretty lit
handleExpr n (Op op) = handleOp n op
handleExpr n (App p q) = case handleExpr n p of
  Expect f   -> f(q)
  Complete s -> Complete s
handleExpr _ (Hole i _) = Complete $ "[" <> pretty i <> "]"

prettyPrec :: Int -> Expr -> Doc ann
prettyPrec n expr = case handleExpr n expr of
  Expect   _ -> mempty
  Complete s -> s

instance Pretty Expr where
  pretty = prettyPrec 0

instance Pretty Lit where
  pretty (Num i) = pretty $ show i
  pretty (Bol b) = pretty $ show b


instance Pretty Op where
  pretty EQ = "="
  pretty LTE = "≤"
  pretty GTE = "≥"
  pretty LT = "<"
  pretty GT = ">"
  pretty Implies = "→"
  pretty Conj = "→"
  pretty Disj = "⋀"
  pretty Neg = "¬"
  pretty Plus = "+"
  pretty Minus = "-"
  pretty Mul = "*"
  pretty Div = "/"


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
