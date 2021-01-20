{-# LANGUAGE OverloadedStrings #-}

module Pretty.Concrete2 where

import Data.Loc
import Data.Text.Prettyprint.Doc
import Pretty.Abstract ()
import Pretty.Util
import Pretty.Variadic
import Syntax.Common (Fixity (..))
import Syntax.Concrete2
import Prelude hiding (Ordering (..))

--------------------------------------------------------------------------------

-- | Program
instance Pretty Program where
  pretty = toDoc . prettyWithLoc

instance PrettyWithLoc Program where
  prettyWithLoc (Program decls stmts l) =
    setLoc l $ mconcat (map prettyWithLoc decls) <> mconcat (map prettyWithLoc stmts)

--------------------------------------------------------------------------------

-- | Declaration
-- instance Pretty Declaration where
--   pretty = toDoc . prettyWithLoc
instance PrettyWithLoc Declaration where
  prettyWithLoc (ConstDecl names t Nothing l) =
    setLoc l $
      "con " <> sepBy ", " (map prettyWithLoc names) <> ": " <> prettyWithLoc t
  prettyWithLoc (ConstDecl names t (Just p) l) =
    setLoc l $
      "con " <> sepBy ", " (map prettyWithLoc names) <> ": " <> prettyWithLoc t
        <> "{ "
        <> prettyWithLoc p
        <> " }"
  prettyWithLoc (VarDecl names t Nothing l) =
    setLoc l $
      "var " <> sepBy ", " (map prettyWithLoc names) <> ": " <> prettyWithLoc t
  prettyWithLoc (VarDecl names t (Just p) l) =
    setLoc l $
      "var " <> sepBy ", " (map prettyWithLoc names) <> ": " <> prettyWithLoc t
        <> "{ "
        <> prettyWithLoc p
        <> " }"
  prettyWithLoc (LetDecl name args expr l) =
    setLoc l $
      "let "
        <> prettyWithLoc name
        <> fromDoc (hsep (map pretty args))
        <> " = "
        <> prettyWithLoc expr

--------------------------------------------------------------------------------

-- | Name
-- instance Pretty Name where
--   pretty = toDoc . prettyWithLoc
instance PrettyWithLoc Name where
  prettyWithLoc (Name n l) = fromLocAndDoc l (pretty n)

--------------------------------------------------------------------------------

-- | Literals
-- instance Pretty Lit where
--   pretty = toDoc . prettyWithLoc
instance PrettyWithLoc Lit where
  prettyWithLoc (LitBool True l) = fromLocAndDoc l "True"
  prettyWithLoc (LitBool False l) = fromLocAndDoc l "False"
  prettyWithLoc (LitInt n l) = fromLocAndDoc l (pretty n)
  prettyWithLoc (LitChar c l) = fromLocAndDoc l (pretty [c])

--------------------------------------------------------------------------------

-- | Operators
instance PrettyWithLoc Op where
  prettyWithLoc (EQ l) = fromLocAndDoc l "="
  prettyWithLoc (NEQ l) = fromLocAndDoc l "/="
  prettyWithLoc (NEQU l) = fromLocAndDoc l "≠"
  prettyWithLoc (LTE l) = fromLocAndDoc l "<="
  prettyWithLoc (LTEU l) = fromLocAndDoc l "≤"
  prettyWithLoc (GTE l) = fromLocAndDoc l ">="
  prettyWithLoc (GTEU l) = fromLocAndDoc l "≥"
  prettyWithLoc (LT l) = fromLocAndDoc l "<"
  prettyWithLoc (GT l) = fromLocAndDoc l ">"
  prettyWithLoc (Implies l) = fromLocAndDoc l "=>"
  prettyWithLoc (ImpliesU l) = fromLocAndDoc l "→"
  prettyWithLoc (Conj l) = fromLocAndDoc l "&&"
  prettyWithLoc (ConjU l) = fromLocAndDoc l "∧"
  prettyWithLoc (Disj l) = fromLocAndDoc l "||"
  prettyWithLoc (DisjU l) = fromLocAndDoc l "∨"
  prettyWithLoc (Neg l) = fromLocAndDoc l "~"
  prettyWithLoc (NegU l) = fromLocAndDoc l "¬"
  prettyWithLoc (Add l) = fromLocAndDoc l "+"
  prettyWithLoc (Sub l) = fromLocAndDoc l "-"
  prettyWithLoc (Mul l) = fromLocAndDoc l "*"
  prettyWithLoc (Div l) = fromLocAndDoc l "/"
  prettyWithLoc (Mod l) = fromLocAndDoc l "%"
  prettyWithLoc (Sum l) = fromLocAndDoc l "Σ"
  prettyWithLoc (Forall l) = fromLocAndDoc l "∀"
  prettyWithLoc (Exists l) = fromLocAndDoc l "∃"

--------------------------------------------------------------------------------

-- | Stmt
-- instance Pretty Stmt where
--   pretty = toDoc . prettyWithLoc
instance PrettyWithLoc Stmt where
  prettyWithLoc (Skip l) = setLoc l "skip"
  prettyWithLoc (Abort l) = setLoc l "abort"
  prettyWithLoc (Assign xs es l) =
    setLoc l $
      sepBy ", " (map prettyWithLoc xs)
        <> ":= "
        <> sepBy ", " (map prettyWithLoc es)
  prettyWithLoc (Assert p l) =
    setLoc l $
      "{ " <> prettyWithLoc p <> " }"
  prettyWithLoc (LoopInvariant p bnd l) =
    setLoc l $
      "{ " <> prettyWithLoc p <> " , bnd: " <> prettyWithLoc bnd <> " }"
  prettyWithLoc (Do gdCmds l) =
    setLoc l $
      "do " <> sepBy "| " (map prettyWithLoc gdCmds) <> "\nod"
  prettyWithLoc (If gdCmds _) =
    "if " <> sepBy "| " (map prettyWithLoc gdCmds) <> "\nfi"
  prettyWithLoc (SpecQM l) = setLoc l "?"
  prettyWithLoc (Spec l) = prettyHole "{!" "!}" l
  prettyWithLoc (Proof l) = prettyHole "{-" "-}" l

instance Pretty GdCmd where
  pretty = toDoc . prettyWithLoc

instance PrettyWithLoc GdCmd where
  prettyWithLoc (GdCmd guard body l) =
    setLoc l $
      prettyWithLoc guard
        <> " ->"
        <> mconcat (map prettyWithLoc body)

--------------------------------------------------------------------------------

-- | Expr
instance Pretty Expr where
  pretty = toDoc . prettyWithLoc

instance PrettyWithLoc Expr where
  prettyWithLoc expr = case handleExpr expr of
    Expect _ -> mempty
    Complete s -> s

handleExpr :: Expr -> Variadic Expr (DocWithLoc ann)
handleExpr (Paren _ NoLoc) = error "NoLoc in Expr.Paren"
handleExpr (Paren x (Loc l m)) = return $ DocWithLoc "(" l l <> prettyWithLoc x <> DocWithLoc ")" m m
handleExpr (Var x _) = return $ prettyWithLoc x
handleExpr (Const x _) = return $ prettyWithLoc x
handleExpr (Lit x _) = return $ prettyWithLoc x
handleExpr (Op x _) = handleOp x
handleExpr (App p q _) = case handleExpr p of
  Expect f -> f q
  Complete s -> do
    t <- handleExpr q
    return $ s <> t
handleExpr (Lam p q l) = return $ setLoc l $ "λ " <> fromLocAndDoc NoLoc (pretty p) <> " → " <> prettyWithLoc q
handleExpr (Quant (s, l) op xs m r n t (e, o) p) =
  return $
    setLoc p $
      fromLocAndDoc l (if s then "⟨" else "<|")
        <> prettyWithLoc op
        <> mconcat (map prettyWithLoc xs)
        <> fromLocAndDoc m ":"
        <> prettyWithLoc r
        <> fromLocAndDoc n ":"
        <> prettyWithLoc t
        <> fromLocAndDoc o (if e then "⟩" else "|>")
handleExpr (Subst _ _) = return "Subst"

handleOp :: Op -> Variadic Expr (DocWithLoc ann)
handleOp op = case classify op of
  Infix _ -> do
    p <- var
    q <- var
    return $
      prettyWithLoc p
        <> prettyWithLoc op
        <> prettyWithLoc q
  InfixL _ -> do
    p <- var
    q <- var
    return $
      prettyWithLoc p
        <> prettyWithLoc op
        <> prettyWithLoc q
  InfixR _ -> do
    p <- var
    q <- var
    return $
      prettyWithLoc p
        <> prettyWithLoc op
        <> prettyWithLoc q
  Prefix _ -> do
    p <- var
    return $ prettyWithLoc op <> prettyWithLoc p
  Postfix _ -> do
    p <- var
    return $ prettyWithLoc p <> prettyWithLoc op

prettyHole :: Doc ann -> Doc ann -> Loc -> DocWithLoc ann
prettyHole left right loc = case loc of
  NoLoc -> fromLocAndDoc loc $ left <> right
  Loc start end -> fromLocAndDoc loc $ left <> fillGap (translate 2 start) (translate (-2) end) <> right

--------------------------------------------------------------------------------

-- | Type
instance Pretty Type where
  pretty = toDoc . prettyWithLoc

instance PrettyWithLoc Type where
  prettyWithLoc (TParen _ NoLoc) = error "NoLoc in Type.TParen"
  prettyWithLoc (TParen t (Loc l m)) =
    DocWithLoc "(" l l <> prettyWithLoc t <> DocWithLoc ")" m m
  prettyWithLoc (TBase TInt l) = setLoc l "Int"
  prettyWithLoc (TBase TBool l) = setLoc l "Bool"
  prettyWithLoc (TBase TChar l) = setLoc l "Char"
  prettyWithLoc (TFunc a (u, m) b l) =
    setLoc l $
      prettyWithLoc a
        <> fromLocAndDoc m (if u then "→" else "->")
        <> prettyWithLoc b
  prettyWithLoc (TArray i b l) =
    setLoc l $
      "array " <> prettyWithLoc i <> "of " <> prettyWithLoc b
  prettyWithLoc (TVar i l) = setLoc l $ "TVar " <> prettyWithLoc i

--------------------------------------------------------------------------------

-- | Interval
-- instance Pretty Interval where
--   pretty = toDoc . prettyWithLoc
instance PrettyWithLoc Interval where
  prettyWithLoc (Interval (Including a) (Including b) l) =
    setLoc l $
      "[" <> prettyWithLoc a <> ".. " <> prettyWithLoc b <> "]"
  prettyWithLoc (Interval (Including a) (Excluding b) l) =
    setLoc l $
      "[" <> prettyWithLoc a <> ".. " <> prettyWithLoc b <> ")"
  prettyWithLoc (Interval (Excluding a) (Including b) l) =
    setLoc l $
      "(" <> prettyWithLoc a <> ".. " <> prettyWithLoc b <> "]"
  prettyWithLoc (Interval (Excluding a) (Excluding b) l) =
    setLoc l $
      "(" <> prettyWithLoc a <> ".. " <> prettyWithLoc b <> ")"