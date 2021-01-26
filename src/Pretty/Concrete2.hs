{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}

module Pretty.Concrete2 where

import Data.Text.Prettyprint.Doc (Pretty (pretty))
import Pretty.Util
import Pretty.Variadic
import Syntax.Common (Fixity (..))
import Syntax.Concrete2
import Syntax.Parser.Lexer (Tok (..))
import Prelude hiding (Ordering (..))

--------------------------------------------------------------------------------

instance (PrettyWithLoc a, PrettyWithLoc b) => PrettyWithLoc (Either a b) where
  prettyWithLoc (Left x) = prettyWithLoc x
  prettyWithLoc (Right x) = prettyWithLoc x

-- | SepBy
instance (PrettyWithLoc (Token sep), PrettyWithLoc a) => PrettyWithLoc (SepBy sep a) where
  prettyWithLoc (Head x) = prettyWithLoc x
  prettyWithLoc (Delim x sep xs) =
    prettyWithLoc x <> prettyWithLoc sep <> prettyWithLoc xs

-- | Tokens
instance PrettyWithLoc (Token 'TokCon) where
  prettyWithLoc (Token l r) = DocWithLoc "con" l r

instance PrettyWithLoc (Token 'TokVar) where
  prettyWithLoc (Token l r) = DocWithLoc "var" l r

instance PrettyWithLoc (Token 'TokLet) where
  prettyWithLoc (Token l r) = DocWithLoc "let" l r

instance PrettyWithLoc (Token 'TokBraceOpen) where
  prettyWithLoc (Token l r) = DocWithLoc "{" l r

instance PrettyWithLoc (Token 'TokBraceClose) where
  prettyWithLoc (Token l r) = DocWithLoc "}" l r

instance PrettyWithLoc (Token 'TokBracketOpen) where
  prettyWithLoc (Token l r) = DocWithLoc "[" l r

instance PrettyWithLoc (Token 'TokBracketClose) where
  prettyWithLoc (Token l r) = DocWithLoc "]" l r

instance PrettyWithLoc (Token 'TokParenOpen) where
  prettyWithLoc (Token l r) = DocWithLoc "(" l r

instance PrettyWithLoc (Token 'TokParenClose) where
  prettyWithLoc (Token l r) = DocWithLoc ")" l r

instance PrettyWithLoc (Token 'TokQuantOpen) where
  prettyWithLoc (Token l r) = DocWithLoc "<|" l r

instance PrettyWithLoc (Token 'TokQuantOpenU) where
  prettyWithLoc (Token l r) = DocWithLoc "⟨" l r

instance PrettyWithLoc (Token 'TokQuantClose) where
  prettyWithLoc (Token l r) = DocWithLoc "|>" l r

instance PrettyWithLoc (Token 'TokQuantCloseU) where
  prettyWithLoc (Token l r) = DocWithLoc "⟩" l r

instance PrettyWithLoc (Token 'TokSpecOpen) where
  prettyWithLoc (Token l r) = DocWithLoc "{!" l r

instance PrettyWithLoc (Token 'TokSpecClose) where
  prettyWithLoc (Token l r) = DocWithLoc "!}" l r

instance PrettyWithLoc (Token 'TokProofOpen) where
  prettyWithLoc (Token l r) = DocWithLoc "{-" l r

instance PrettyWithLoc (Token 'TokProofClose) where
  prettyWithLoc (Token l r) = DocWithLoc "-}" l r
  
instance PrettyWithLoc (Token 'TokColon) where
  prettyWithLoc (Token l r) = DocWithLoc ":" l r

instance PrettyWithLoc (Token 'TokComma) where
  prettyWithLoc (Token l r) = DocWithLoc "," l r

instance PrettyWithLoc (Token 'TokRange) where
  prettyWithLoc (Token l r) = DocWithLoc ".." l r

instance PrettyWithLoc (Token 'TokArray) where
  prettyWithLoc (Token l r) = DocWithLoc "array" l r

instance PrettyWithLoc (Token 'TokOf) where
  prettyWithLoc (Token l r) = DocWithLoc "of" l r

instance PrettyWithLoc (Token 'TokBnd) where
  prettyWithLoc (Token l r) = DocWithLoc "bnd" l r

instance PrettyWithLoc (Token 'TokIf) where
  prettyWithLoc (Token l r) = DocWithLoc "if" l r

instance PrettyWithLoc (Token 'TokFi) where
  prettyWithLoc (Token l r) = DocWithLoc "fi" l r

instance PrettyWithLoc (Token 'TokDo) where
  prettyWithLoc (Token l r) = DocWithLoc "do" l r

instance PrettyWithLoc (Token 'TokOd) where
  prettyWithLoc (Token l r) = DocWithLoc "od" l r

instance PrettyWithLoc (Token 'TokAssign) where
  prettyWithLoc (Token l r) = DocWithLoc ":=" l r

instance PrettyWithLoc (Token 'TokEQ) where
  prettyWithLoc (Token l r) = DocWithLoc "=" l r

instance PrettyWithLoc (Token 'TokGuardBar) where
  prettyWithLoc (Token l r) = DocWithLoc "|" l r

instance PrettyWithLoc (Token 'TokArrow) where
  prettyWithLoc (Token l r) = DocWithLoc "->" l r

instance PrettyWithLoc (Token 'TokArrowU) where
  prettyWithLoc (Token l r) = DocWithLoc "→" l r

--------------------------------------------------------------------------------

-- | Program
instance Pretty Program where
  pretty = toDoc . prettyWithLoc

instance PrettyWithLoc Program where
  prettyWithLoc (Program decls stmts) =
    mconcat (map prettyWithLoc decls) <> mconcat (map prettyWithLoc stmts)

--------------------------------------------------------------------------------

-- | Declaration
instance Pretty Declaration where
  pretty = toDoc . prettyWithLoc

instance PrettyWithLoc Declaration where
  prettyWithLoc (ConstDecl con names colon t) =
    prettyWithLoc con
      <> prettyWithLoc names
      <> prettyWithLoc colon
      <> prettyWithLoc t
  prettyWithLoc (ConstDeclWithProp con names colon t l p r) =
    prettyWithLoc con
      <> prettyWithLoc names
      <> prettyWithLoc colon
      <> prettyWithLoc t
      <> prettyWithLoc l
      <> prettyWithLoc p
      <> prettyWithLoc r
  prettyWithLoc (VarDecl v names colon t) =
    prettyWithLoc v <> prettyWithLoc names <> prettyWithLoc colon <> prettyWithLoc t
  prettyWithLoc (VarDeclWithProp v names colon t l p r) =
    prettyWithLoc v <> prettyWithLoc names <> prettyWithLoc colon <> prettyWithLoc t
      <> prettyWithLoc l
      <> prettyWithLoc p
      <> prettyWithLoc r
  prettyWithLoc (LetDecl a name args e expr) =
    prettyWithLoc a
      <> prettyWithLoc name
      <> mconcat (map prettyWithLoc args)
      <> prettyWithLoc e
      <> prettyWithLoc expr

--------------------------------------------------------------------------------

-- | Name
-- instance Pretty Name where
--   pretty = toDoc . prettyWithLoc
instance PrettyWithLoc Name where
  prettyWithLoc (Name n l) = fromDoc l (pretty n)

--------------------------------------------------------------------------------

-- | Literals
-- instance Pretty Lit where
--   pretty = toDoc . prettyWithLoc
instance PrettyWithLoc Lit where
  prettyWithLoc (LitBool True l) = fromDoc l "True"
  prettyWithLoc (LitBool False l) = fromDoc l "False"
  prettyWithLoc (LitInt n l) = fromDoc l (pretty n)
  prettyWithLoc (LitChar c l) = fromDoc l (pretty [c])

--------------------------------------------------------------------------------

-- | Operators
instance PrettyWithLoc Op where
  prettyWithLoc (EQ l) = fromDoc l "="
  prettyWithLoc (NEQ l) = fromDoc l "/="
  prettyWithLoc (NEQU l) = fromDoc l "≠"
  prettyWithLoc (LTE l) = fromDoc l "<="
  prettyWithLoc (LTEU l) = fromDoc l "≤"
  prettyWithLoc (GTE l) = fromDoc l ">="
  prettyWithLoc (GTEU l) = fromDoc l "≥"
  prettyWithLoc (LT l) = fromDoc l "<"
  prettyWithLoc (GT l) = fromDoc l ">"
  prettyWithLoc (Implies l) = fromDoc l "=>"
  prettyWithLoc (ImpliesU l) = fromDoc l "→"
  prettyWithLoc (Conj l) = fromDoc l "&&"
  prettyWithLoc (ConjU l) = fromDoc l "∧"
  prettyWithLoc (Disj l) = fromDoc l "||"
  prettyWithLoc (DisjU l) = fromDoc l "∨"
  prettyWithLoc (Neg l) = fromDoc l "~"
  prettyWithLoc (NegU l) = fromDoc l "¬"
  prettyWithLoc (Add l) = fromDoc l "+"
  prettyWithLoc (Sub l) = fromDoc l "-"
  prettyWithLoc (Mul l) = fromDoc l "*"
  prettyWithLoc (Div l) = fromDoc l "/"
  prettyWithLoc (Mod l) = fromDoc l "%"
  prettyWithLoc (Sum l) = fromDoc l "Σ"
  prettyWithLoc (Forall l) = fromDoc l "∀"
  prettyWithLoc (Exists l) = fromDoc l "∃"

--------------------------------------------------------------------------------

-- | Stmt
instance Pretty Stmt where
  pretty = toDoc . prettyWithLoc

instance PrettyWithLoc Stmt where
  prettyWithLoc (Skip l) = fromDoc l "skip"
  prettyWithLoc (Abort l) = fromDoc l "abort"
  prettyWithLoc (Assign xs a es) =
    prettyWithLoc xs <> prettyWithLoc a <> prettyWithLoc es
  prettyWithLoc (Assert l p r) =
    prettyWithLoc l
      <> prettyWithLoc p
      <> prettyWithLoc r
  prettyWithLoc (LoopInvariant l p c b bnd d r) =
    prettyWithLoc l
      <> prettyWithLoc p
      <> prettyWithLoc c
      <> prettyWithLoc b
      <> prettyWithLoc bnd
      <> prettyWithLoc d
      <> prettyWithLoc r
  prettyWithLoc (Do l gdCmds r) =
    prettyWithLoc l
      <> prettyWithLoc gdCmds
      <> prettyWithLoc r
  prettyWithLoc (If l gdCmds r) =
    prettyWithLoc l
      <> prettyWithLoc gdCmds
      <> prettyWithLoc r
  prettyWithLoc (SpecQM l) = fromDoc l "?"
  prettyWithLoc (Spec l r) = prettyWithLoc l <> prettyWithLoc r
  prettyWithLoc (Proof l r) = prettyWithLoc l <> prettyWithLoc r

instance Pretty GdCmd where
  pretty = toDoc . prettyWithLoc

instance PrettyWithLoc GdCmd where
  prettyWithLoc (GdCmd guard a body) =
    prettyWithLoc guard
      <> prettyWithLoc a
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
handleExpr (Paren l x r) =
  return $
    prettyWithLoc l
      <> prettyWithLoc x
      <> prettyWithLoc r
handleExpr (Var x) = return $ prettyWithLoc x
handleExpr (Const x) = return $ prettyWithLoc x
handleExpr (Lit x) = return $ prettyWithLoc x
handleExpr (Op x) = handleOp x
handleExpr (App p q) = case handleExpr p of
  Expect f -> f q
  Complete s -> do
    t <- handleExpr q
    return $ s <> t
handleExpr (Quant open op xs m r n t close) =
  return $
    prettyWithLoc open
      <> prettyWithLoc op
      <> mconcat (map prettyWithLoc xs)
      <> prettyWithLoc m
      <> prettyWithLoc r
      <> prettyWithLoc n
      <> prettyWithLoc t
      <> prettyWithLoc close

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

--------------------------------------------------------------------------------

-- | Type
instance Pretty Type where
  pretty = toDoc . prettyWithLoc

instance PrettyWithLoc Type where
  prettyWithLoc (TParen l t r) = prettyWithLoc l <> prettyWithLoc t <> prettyWithLoc r
  -- DocWithLoc "(" l l <> prettyWithLoc t <> DocWithLoc ")" m m
  prettyWithLoc (TBase (TInt l)) = fromDoc l "Int"
  prettyWithLoc (TBase (TBool l)) = fromDoc l "Bool"
  prettyWithLoc (TBase (TChar l)) = fromDoc l "Char"
  prettyWithLoc (TFunc a l b) = prettyWithLoc a <> prettyWithLoc l <> prettyWithLoc b
  prettyWithLoc (TArray l a r b) =
    prettyWithLoc l <> prettyWithLoc a <> prettyWithLoc r
      <> prettyWithLoc b
  prettyWithLoc (TVar i) = prettyWithLoc i

--------------------------------------------------------------------------------

-- | Endpoint & Interval
instance PrettyWithLoc EndpointOpen where
  prettyWithLoc (IncludingOpening l e) = prettyWithLoc l <> prettyWithLoc e
  prettyWithLoc (ExcludingOpening l e) = prettyWithLoc l <> prettyWithLoc e

instance PrettyWithLoc EndpointClose where
  prettyWithLoc (IncludingClosing e l) = prettyWithLoc e <> prettyWithLoc l
  prettyWithLoc (ExcludingClosing e l) = prettyWithLoc e <> prettyWithLoc l

instance PrettyWithLoc Interval where
  prettyWithLoc (Interval a b c) = prettyWithLoc a <> prettyWithLoc b <> prettyWithLoc c
