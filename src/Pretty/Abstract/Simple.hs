{-# LANGUAGE DeriveFunctor, OverloadedStrings #-}

module Pretty.Abstract.Simple where

import Data.Text.Prettyprint.Doc
import Control.Monad ((>=>))
import Prelude hiding (Ordering(..))

import Syntax.Abstract.Simple hiding (var)
import Syntax.Abstract (Fixity(..))

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
handleExpr _ (Quant a x b c) = return $ pretty a <+> pretty x <+> pretty b <+> pretty c

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
  pretty Conj = "∧"
  pretty Disj = "∨"
  pretty Neg = "¬"
  pretty Add = "+"
  pretty Sub = "-"
  pretty Mul = "*"
  pretty Div = "/"
  pretty Mod = "%"

--------------------------------------------------------------------------------
-- | Type

-- instance Pretty Endpoint where
--   pretty (Including e) = ""
instance Pretty Interval where
  pretty (Interval (Including a) (Including b)) =
      "[" <+> pretty a <+> ".." <+> pretty b <+> "]"
  pretty (Interval (Including a) (Excluding b)) =
      "[" <+> pretty a <+> ".." <+> pretty b <+> ")"
  pretty (Interval (Excluding a) (Including b)) =
      "(" <+> pretty a <+> ".." <+> pretty b <+> "]"
  pretty (Interval (Excluding a) (Excluding b)) =
      "(" <+> pretty a <+> ".." <+> pretty b <+> ")"

instance Pretty Type where
  pretty (TBase TInt) = "Int"
  pretty (TBase TBool) = "Bool"
  pretty (TBase TChar) = "Char"
  pretty (TFunc a b) = pretty a <+> "->" <+> pretty b
  pretty (TArray i b) = "array" <+> pretty i <+> "of" <+> pretty b
  pretty (TVar i) = "TVar" <+> pretty i
