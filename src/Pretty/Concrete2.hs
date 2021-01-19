{-# LANGUAGE OverloadedStrings #-}

module Pretty.Concrete2 where

import Data.Loc
import Data.Text.Prettyprint.Doc
import Pretty.Abstract ()
import Pretty.Util
import Pretty.Variadic
import Syntax.Abstract (Fixity (..), classify)
import Syntax.Concrete2 hiding (GT)

--------------------------------------------------------------------------------

-- | Program
instance Pretty Program where
  pretty = toDoc . prettyWithLoc
instance PrettyWithLoc Program where
  prettyWithLoc (Program decls _ _ stmts l) = setLoc l $ 
    mconcat (map prettyWithLoc decls) <> mconcat (map prettyWithLoc stmts)

--------------------------------------------------------------------------------
-- | Declaration

instance Pretty Declaration where
  pretty = toDoc . prettyWithLoc

instance PrettyWithLoc Declaration where
  prettyWithLoc (ConstDecl names t Nothing l) = setLoc l $
    "con " <> sepBy ", " (map prettyWithLoc names) <> ": " <> prettyWithLoc t 
  prettyWithLoc (ConstDecl names t (Just p) l) = setLoc l $
    "con " <> sepBy ", " (map prettyWithLoc names) <> ": " <> prettyWithLoc t 
      <> "{ " <> prettyWithLoc p <> " }"
  prettyWithLoc (VarDecl names t Nothing l) = setLoc l $
    "var " <> sepBy ", " (map prettyWithLoc names) <> ": " <> prettyWithLoc t 
  prettyWithLoc (VarDecl names t (Just p) l) = setLoc l $
    "var " <> sepBy ", " (map prettyWithLoc names) <> ": " <> prettyWithLoc t 
      <> "{ " <> prettyWithLoc p <> " }"
  prettyWithLoc (LetDecl name args expr l) = setLoc l $
    "let " 
      <> prettyWithLoc name 
      <> fromDoc (hsep (map pretty args)) 
      <> " = " 
      <> prettyWithLoc expr 

--------------------------------------------------------------------------------

-- | Name
instance Pretty Name where
  pretty = toDoc . prettyWithLoc

instance PrettyWithLoc Name where
  prettyWithLoc (Name n l) = fromLocAndDoc l (pretty n)

--------------------------------------------------------------------------------

-- | Literals
instance Pretty Lit where
  pretty = toDoc . prettyWithLoc

instance PrettyWithLoc Lit where
  prettyWithLoc (LitBool True l) = fromLocAndDoc l "True"
  prettyWithLoc (LitBool False l) = fromLocAndDoc l "False"
  prettyWithLoc (LitInt n l) = fromLocAndDoc l (pretty n)
  prettyWithLoc (LitChar c l) = fromLocAndDoc l (pretty [c])

--------------------------------------------------------------------------------

-- | Stmt
instance Pretty Stmt where
  pretty = toDoc . prettyWithLoc

instance PrettyWithLoc Stmt where
  prettyWithLoc (Skip l) = setLoc l "skip"
  prettyWithLoc (Abort l) = setLoc l "abort"
  prettyWithLoc (Assign xs es l) =
    setLoc l $
      sepBy ", " (map prettyWithLoc xs)
        <> ":= "
        <> sepBy ", " (map prettyWithLoc es)
  prettyWithLoc (Assert p l) = setLoc l $ 
    "{ " <> prettyWithLoc p <> " }"
  prettyWithLoc (LoopInvariant p bnd l) = setLoc l $ 
    "{ " <> prettyWithLoc p <> " , bnd: " <> prettyWithLoc bnd <> " }"
  prettyWithLoc (Do gdCmds l) = setLoc l $ 
    "do " <> sepBy "| " (map prettyWithLoc gdCmds) <> "\nod"
  prettyWithLoc (If gdCmds _) =
    "if " <> sepBy "| " (map prettyWithLoc gdCmds) <> "\nfi"
  prettyWithLoc (SpecQM l) = setLoc l "?"
  prettyWithLoc (Spec l) = prettyHole "{!" "!}" l
  prettyWithLoc (Proof l) = prettyHole "{-" "-}" l

instance Pretty GdCmd where
  pretty = toDoc . prettyWithLoc

instance PrettyWithLoc GdCmd where
  prettyWithLoc (GdCmd guard body l) = setLoc l $ 
    prettyWithLoc guard 
    <> " ->" 
    <> mconcat (map prettyWithLoc body)

--------------------------------------------------------------------------------
-- | Expr
instance Pretty Expr where
  pretty = toDoc . prettyWithLoc

instance PrettyPrec Expr where
  prettyPrec _ = toDoc . prettyPrecWithLoc 0

instance PrettyWithLoc Expr where
  prettyWithLoc = prettyPrecWithLoc 0

instance PrettyPrecWithLoc Expr where
  prettyPrecWithLoc n expr = case handleExpr n expr of
    Expect _ -> mempty
    Complete s -> s

handleExpr :: Int -> Expr -> Variadic Expr (DocWithLoc ann)
handleExpr _ (Paren x l) = return $ prettyWithLoc x
handleExpr _ (Var x _) = return $ prettyWithLoc x
handleExpr _ (Const x _) = return $ prettyWithLoc x
handleExpr _ (Lit x l) = return $ fromLocAndDoc l (pretty x)
handleExpr n (Op x l) = handleOp n x l
handleExpr n (App p q _) = case handleExpr n p of
  Expect f -> f q
  Complete s -> do
    t <- handleExpr n q
    -- see if the second argument is an application, apply parenthesis when needed
    return $ case q of
      App {} -> s <> parensIf' n 0 t
      _ -> s <> t
handleExpr _ (Lam p q l) = return $ setLoc l $ "λ " <> fromLocAndDoc NoLoc (pretty p) <> " → " <> prettyWithLoc q
handleExpr _ (Hole l) = return $ prettyHole "{!" "!}" l
handleExpr _ (Quant op xs r t l) =
  return $
    setLoc l $
      "⟨"
        <> prettyWithLoc op
        <> mconcat (map prettyWithLoc xs)
        <> " : "
        <> prettyWithLoc r
        <> " : "
        <> prettyWithLoc t
        <> " ⟩"
handleExpr _ (Subst _ _) = return "Subst"

handleOp :: Int -> Op -> Loc -> Variadic Expr (DocWithLoc ann)
handleOp n op loc = case classify op of
  Infix m -> do
    p <- var
    q <- var
    return $
      parensIf' n m $
        prettyPrecWithLoc (succ m) p
          <> fromLocAndDoc loc (pretty op)
          <> prettyPrecWithLoc (succ m) q
  InfixL m -> do
    p <- var
    q <- var
    return $
      parensIf' n m $
        prettyPrecWithLoc m p
          <> fromLocAndDoc loc (pretty op)
          <> prettyPrecWithLoc (succ m) q
  InfixR m -> do
    p <- var
    q <- var
    return $
      parensIf' n m $
        prettyPrecWithLoc (succ m) p
          <> fromLocAndDoc loc (pretty op)
          <> prettyPrecWithLoc m q
  Prefix m -> do
    p <- var
    return $ parensIf' n m $ fromLocAndDoc loc (pretty op) <> prettyPrecWithLoc m p
  Postfix m -> do
    p <- var
    return $ parensIf' n m $ prettyPrecWithLoc m p <> fromLocAndDoc loc (pretty op)

prettyHole :: Doc ann -> Doc ann -> Loc -> DocWithLoc ann
prettyHole left right loc = case loc of
  NoLoc -> fromLocAndDoc loc $ left <> right
  Loc start end -> fromLocAndDoc loc $ left <> fillGap (translate 2 start) (translate (-2) end) <> right

--------------------------------------------------------------------------------
-- | Type

instance Pretty Type where
  pretty = toDoc . prettyWithLoc

instance PrettyWithLoc Type where
  prettyWithLoc (TBase TInt  l) = setLoc l $ "Int"
  prettyWithLoc (TBase TBool l) = setLoc l $ "Bool"
  prettyWithLoc (TBase TChar l) = setLoc l $ "Char"
  prettyWithLoc (TFunc a b   l) = setLoc l $ prettyWithLoc a <> "→ " <> prettyWithLoc b
  prettyWithLoc (TArray i b  l) = setLoc l $ "array " <> prettyWithLoc i <> "of " <> prettyWithLoc b
  prettyWithLoc (TVar i      l) = setLoc l $ "TVar " <> prettyWithLoc i

--------------------------------------------------------------------------------
-- | Interval

instance Pretty Interval where
  pretty = toDoc . prettyWithLoc

instance PrettyWithLoc Interval where
  prettyWithLoc (Interval (Including a) (Including b) l) = setLoc l $ 
    "[" <> prettyWithLoc a <> ".. " <> prettyWithLoc b <> "]"
  prettyWithLoc (Interval (Including a) (Excluding b) l) = setLoc l $ 
    "[" <> prettyWithLoc a <> ".. " <> prettyWithLoc b <> ")"
  prettyWithLoc (Interval (Excluding a) (Including b) l) = setLoc l $ 
    "(" <> prettyWithLoc a <> ".. " <> prettyWithLoc b <> "]"
  prettyWithLoc (Interval (Excluding a) (Excluding b) l) = setLoc l $ 
    "(" <> prettyWithLoc a <> ".. " <> prettyWithLoc b <> ")"