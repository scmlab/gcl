{-# LANGUAGE OverloadedStrings #-}

module Pretty.Abstract where

import Data.Text.Prettyprint.Doc
import Pretty.Util
import Pretty.Variadic
import Pretty.Common ()
import Syntax.Abstract
import Syntax.Common
import Prelude hiding (Ordering (..))
import Data.Loc ((<-->))

--------------------------------------------------------------------------------

-- | Program
instance Pretty Program where
  pretty (Program decls _ _ stmts _) = vsep $ map pretty decls ++ map pretty stmts

--------------------------------------------------------------------------------

-- | Declaration
instance Pretty Declaration where
  pretty (ConstDecl names t Nothing _) =
    "con "
      <> hsep (punctuate ", " (map pretty names))
      <> ": "
      <> pretty t
  pretty (ConstDecl names t (Just p) _) =
    "con "
      <> hsep (punctuate ", " (map pretty names))
      <> ": "
      <> pretty t
      <> "{ "
      <> pretty p
      <> " }"
  pretty (VarDecl names t Nothing _) =
    "var "
      <> hsep (punctuate ", " (map pretty names))
      <> ": "
      <> pretty t
  pretty (VarDecl names t (Just p) _) =
    "var "
      <> hsep (punctuate ", " (map pretty names))
      <> ": "
      <> pretty t
      <> "{ "
      <> pretty p
      <> " }"
  pretty (LetDecl name args expr _) =
    "let "
      <> pretty name
      <> hsep (map pretty args)
      <> " = "
      <> pretty expr

--------------------------------------------------------------------------------

-- | Name
instance Pretty Name where
  pretty (Name n _) = pretty n

--------------------------------------------------------------------------------

-- | Stmt
instance Pretty Stmt where
  pretty (Skip _) = "skip"
  pretty (Abort _) = "abort"
  pretty (Assign xs es _) =
    hsep (punctuate ", " (map pretty xs))
      <> ":= "
      <> hsep (punctuate ", " (map pretty es))
  pretty (Assert p _) =
    "{ " <> pretty p <> " }"
  pretty (LoopInvariant p bnd _) =
    "{ " <> pretty p <> " , bnd: " <> pretty bnd <> " }"
  pretty (Do gdCmds _) =
    "do"
      <> line
      <> vsep (map (\x -> " |" <+> pretty x <> line) gdCmds)
      <> "od"
  pretty (If gdCmds _) =
    "if"
      <> line
      <> vsep (map (\x -> " |" <+> pretty x <> line) gdCmds)
      <> "fi"
  pretty (Spec content _) = "[!" <> pretty content <> "!]"
  pretty (Proof _) = "{-  -}"

instance Pretty GdCmd where
  pretty (GdCmd guard body _) =
    pretty guard
      <+> "->"
      <+> vsep (map pretty body)

--------------------------------------------------------------------------------

-- | Literals
instance Pretty Lit where
  pretty (Num i) = pretty $ show i
  pretty (Bol b) = pretty $ show b
  pretty (Chr c) = pretty $ show c

--------------------------------------------------------------------------------

-- | Expr
instance Pretty Expr where
  pretty = prettyPrec 0

instance PrettyPrec Expr where
  prettyPrec n expr = case handleExpr n expr of
    Expect _ -> mempty
    Complete s -> s

handleExpr :: Int -> Expr -> Variadic Expr (Doc ann)
handleExpr _ (Var x _) = return $ pretty x
handleExpr _ (Const x _) = return $ pretty x
handleExpr _ (Lit x _) = return $ pretty x
handleExpr n (Op x) = handleOp n x
handleExpr _ (Chain a op b _) = 
  return $ pretty a
    <> pretty op
    <> pretty b
handleExpr n (App p q _) = case handleExpr n p of
  Expect f -> f q
  Complete s -> do
    t <- handleExpr n q
    -- see if the second argument is an application, apply parenthesis when needed
    return $ case q of
      App {} -> s <> parensIf n 0 t
      _ -> s <> t
handleExpr _ (Lam p q _) = return $ "λ" <+> pretty p <+> "→" <+> pretty q
handleExpr _ (Hole _) = return "{!!}"
handleExpr _ (Quant op xs r t _) =
  return $
    "⟨"
      <> pretty op
      <> mconcat (map pretty xs)
      <> " : "
      <> pretty r
      <> " : "
      <> pretty t
      <> " ⟩"
handleExpr _ (Subst _ _) = return "Subst"

--------------------------------------------------------------------------------

handleOp :: Int -> Op -> Variadic Expr (Doc ann)
handleOp n op = case classify op of
  Infix m -> do
    p <- var
    q <- var
    return $
      parensIf n m $
        prettyPrec (succ m) p
          <+> pretty op
          <+> prettyPrec (succ m) q
  InfixL m -> do
    p <- var
    q <- var
    return $
      parensIf n m $
        prettyPrec m p
          <+> pretty op
          <+> prettyPrec (succ m) q
  InfixR m -> do
    p <- var
    q <- var
    return $
      parensIf n m $
        prettyPrec (succ m) p
          <+> pretty op
          <+> prettyPrec m q
  Prefix m -> do
    p <- var
    return $ parensIf n m $ pretty op <+> prettyPrec m p
  Postfix m -> do
    p <- var
    return $ parensIf n m $ prettyPrec m p <+> pretty op

--------------------------------------------------------------------------------

-- | Type
instance Pretty Type where
  pretty (TBase TInt _) = "Int"
  pretty (TBase TBool _) = "Bool"
  pretty (TBase TChar _) = "Char"
  pretty (TFunc a b _) = pretty a <+> "→" <+> pretty b
  pretty (TArray i b _) = "array" <+> pretty i <+> "of" <+> pretty b
  pretty (TVar i _) = "TVar" <+> pretty i

--------------------------------------------------------------------------------

-- | Interval
instance Pretty Interval where
  pretty (Interval (Including a) (Including b) _) =
    "[" <+> pretty a <+> ".." <+> pretty b <+> "]"
  pretty (Interval (Including a) (Excluding b) _) =
    "[" <+> pretty a <+> ".." <+> pretty b <+> ")"
  pretty (Interval (Excluding a) (Including b) _) =
    "(" <+> pretty a <+> ".." <+> pretty b <+> "]"
  pretty (Interval (Excluding a) (Excluding b) _) =
    "(" <+> pretty a <+> ".." <+> pretty b <+> ")"