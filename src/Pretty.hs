{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE TypeSynonymInstances, FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Pretty where

import Data.Text.Prettyprint.Doc
import Control.Monad ((>=>))
import Prelude hiding (Ordering(..))
import Data.Loc
import Text.Megaparsec.Error (errorBundlePretty)


import Error
import Syntax.Parser.Lexer (LexicalError)
import Syntax.Parser (SyntacticError(..))
import Syntax.Abstract hiding (var)
-- import Syntax.Abstract (Expr(..), Lit(..), Op(..), classify, Type(..), TBase(..))
import Syntax.Concrete (Fixity(..))
import GCL.PreCond (Obligation(..), Specification(..))
import GCL.Type (TypeError(..))
import GCL.Exec.ExecMonad (Val(..))

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
  pretty Conj = "→"
  pretty Disj = "⋀"
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
-- | Error

instance Pretty Error where
  pretty (LexicalError err) = "Lexical Error" <+> pretty err
  pretty (SyntacticError (loc, err)) = "Syntactic Error" <+> pretty loc <> line
    <> pretty err
  pretty (TypeError err) = "Type Error" <+> pretty (locOf err) <> line <> pretty err
  pretty (ConvertError err) = "AST Convert Error" <+> pretty (locOf err) <> line <> pretty err

instance Pretty LexicalError where
instance Pretty ConvertError where

instance Pretty TypeError where
  pretty (NotInScope name _) = "The definition" <+> pretty name <+> "is not in scope"
  pretty (UnifyFailed a b _) = "Cannot unify:"
    <+> pretty a <+> "with"
    <+> pretty b
  pretty (RecursiveType v a _) = "Recursive type variable: "
    <+> pretty v <+> "in" <+> pretty a
  pretty (NotFunction a _) = "The type" <+> pretty a <+> "is not a function type"

--------------------------------------------------------------------------------
-- | Val

instance Pretty Loc where
  pretty = pretty . displayLoc

--------------------------------------------------------------------------------
-- | Misc

instance Pretty Val where
  pretty = pretty . show 
