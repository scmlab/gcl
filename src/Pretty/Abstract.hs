{-# LANGUAGE OverloadedStrings #-}

module Pretty.Abstract where

import           Data.Text.Prettyprint.Doc
import           Prelude                 hiding ( Ordering(..) )

import           Pretty.Variadic
import           Syntax.Abstract         hiding ( var )
import           Pretty.Util
import qualified Data.Map as Map

--------------------------------------------------------------------------------
-- | Helper functions 

prettySubst :: Subst -> Doc ann
prettySubst = encloseSep lbracket rbracket comma  . map (\(k, v) -> pretty k <> "/" <> pretty v) . Map.toList

--------------------------------------------------------------------------------
-- | Expr

handleOp :: Int -> Op -> Variadic Expr (Doc ann)
handleOp n op = case classify op of
  Infix m -> do
    p <- var
    q <- var
    return
      $   parensIf n m
      $   prettyPrec (succ m) p
      <+> pretty op
      <+> prettyPrec (succ m) q
  InfixL m -> do
    p <- var
    q <- var
    return
      $   parensIf n m
      $   prettyPrec m p
      <+> pretty op
      <+> prettyPrec (succ m) q
  InfixR m -> do
    p <- var
    q <- var
    return
      $   parensIf n m
      $   prettyPrec (succ m) p
      <+> pretty op
      <+> prettyPrec m q
  Prefix m -> do
    p <- var
    return $ parensIf n m $ pretty op <+> prettyPrec m p
  Postfix m -> do
    p <- var
    return $ parensIf n m $ prettyPrec m p <+> pretty op

handleExpr :: Int -> Expr -> Variadic Expr (Doc ann)
handleExpr _ (Var   v  ) = return $ pretty v
handleExpr _ (Const c  ) = return $ pretty c
handleExpr _ (Lit   lit) = return $ pretty lit
handleExpr n (Op    op ) = handleOp n op
handleExpr n (App p q  ) = case handleExpr n p of
  Expect   f -> f q
  Complete s -> do
    t <- handleExpr n q
    -- see if the second argument is an application, apply parenthesis when needed
    case q of
      App _ _ -> return $ s <+> parensIf n 0 t
      _       -> return $ s <+> t
handleExpr _ (Quant op xs r t) =
  return
    $   "⟨"
    <>  prettyQuantOp op
    <+> hsep (map pretty xs)
    <+> ":"
    <+> pretty r
    <+> ":"
    <+> pretty t
    <>  "⟩"

handleExpr _ (Hole i _) = return $ "[" <> pretty i <> "]"
handleExpr _ (Subst expr env) = return $ pretty expr <> prettySubst env 

instance PrettyPrec Expr where
  prettyPrec n expr = case handleExpr n expr of
    Expect   _ -> mempty
    Complete s -> s

-- SCM: used for printing the operator in quantifier. Temporary.
prettyQuantOp :: Expr -> Doc ann
prettyQuantOp (Op Add ) = "Σ"
prettyQuantOp (Op Conj) = "∀"
prettyQuantOp (Op Disj) = "∃"
prettyQuantOp (Op op  ) = pretty op
prettyQuantOp other     = pretty other

instance Pretty Expr where
  pretty = prettyPrec 0

instance Pretty Lit where
  pretty (Num i) = pretty $ show i
  pretty (Bol b) = pretty $ show b
  pretty (Chr c) = pretty $ show c


instance Pretty Op where
  pretty EQ      = "="
  pretty NEQ     = "≠"
  pretty LTE     = "≤"
  pretty GTE     = "≥"
  pretty LT      = "<"
  pretty GT      = ">"
  pretty Implies = "→"
  pretty Conj    = "∧"
  pretty Disj    = "∨"
  pretty Neg     = "¬"
  pretty Add     = "+"
  pretty Sub     = "-"
  pretty Mul     = "*"
  pretty Div     = "/"
  pretty Mod     = "%"
  pretty Sum     = "Σ"
  pretty Forall  = "∀"
  pretty Exists  = "∃"

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
  pretty (TBase TInt ) = "Int"
  pretty (TBase TBool) = "Bool"
  pretty (TBase TChar) = "Char"
  pretty (TFunc  a b ) = pretty a <+> "->" <+> pretty b
  pretty (TArray i b ) = "array" <+> pretty i <+> "of" <+> pretty b
  pretty (TVar i     ) = "TVar" <+> pretty i
