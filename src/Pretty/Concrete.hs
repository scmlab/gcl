{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}

module Pretty.Concrete where

import           Data.Loc                       ( locOf )
import           Prelude                 hiding ( Ordering(..) )
import           Pretty.Common                  ( )
import           Pretty.Util
import           Pretty.Variadic
import           Prettyprinter                  ( Pretty(pretty) )
import           Syntax.Common
import           Syntax.Concrete
import           Syntax.Parser.Lexer (Tok(..))
import           Data.Text (unpack)

--------------------------------------------------------------------------------

-- | SepBy
instance (PrettyWithLoc (Token sep), PrettyWithLoc a) => PrettyWithLoc (SepBy sep a) where
  prettyWithLoc (Head x) = prettyWithLoc x
  prettyWithLoc (Delim x sep xs) =
    prettyWithLoc x <> prettyWithLoc sep <> prettyWithLoc xs

-- | Tokens
instance PrettyWithLoc (Token "do") where
  prettyWithLoc (Token l r) = DocWithLoc (pretty $ show TokDo) l r

instance PrettyWithLoc (Token "od") where
  prettyWithLoc (Token l r) = DocWithLoc (pretty $ show TokOd) l r

instance PrettyWithLoc (Token "if") where
  prettyWithLoc (Token l r) = DocWithLoc (pretty $ show TokIf) l r

instance PrettyWithLoc (Token "fi") where
  prettyWithLoc (Token l r) = DocWithLoc (pretty $ show TokFi) l r

instance PrettyWithLoc (Token "bnd") where
  prettyWithLoc (Token l r) = DocWithLoc (pretty $ show TokBnd) l r

instance PrettyWithLoc (Token "con") where
  prettyWithLoc (Token l r) = DocWithLoc (pretty $ show TokCon) l r

instance PrettyWithLoc (Token "var") where
  prettyWithLoc (Token l r) = DocWithLoc (pretty $ show TokVar) l r

instance PrettyWithLoc (Token "data") where
  prettyWithLoc (Token l r) = DocWithLoc (pretty $ show TokData) l r

instance PrettyWithLoc (Token "array") where
  prettyWithLoc (Token l r) = DocWithLoc (pretty $ show TokArray) l r

instance PrettyWithLoc (Token "new") where
  prettyWithLoc (Token l r) = DocWithLoc (pretty $ show TokNew) l r

instance PrettyWithLoc (Token "dispose") where
  prettyWithLoc (Token l r) = DocWithLoc (pretty $ show TokDispose) l r

instance PrettyWithLoc (Token "..") where
  prettyWithLoc (Token l r) = DocWithLoc (pretty $ show TokRange) l r

instance PrettyWithLoc (Token "|") where
  prettyWithLoc (Token l r) = DocWithLoc (pretty $ show TokGuardBar) l r

instance PrettyWithLoc (Token "->") where
  prettyWithLoc (Token l r) = DocWithLoc (pretty $ show TokArrow) l r

instance PrettyWithLoc (Token "→") where
  prettyWithLoc (Token l r) = DocWithLoc (pretty $ show TokArrowU) l r

instance PrettyWithLoc (Token "case") where
  prettyWithLoc (Token l r) = DocWithLoc (pretty $ show TokCase) l r

instance PrettyWithLoc (Token "of") where
  prettyWithLoc (Token l r) = DocWithLoc (pretty $ show TokOf) l r

instance PrettyWithLoc (Token "*") where
  prettyWithLoc (Token l r) = DocWithLoc (pretty $ show TokMul) l r

instance PrettyWithLoc (Token "=") where
  prettyWithLoc (Token l r) = DocWithLoc (pretty $ show TokEQ) l r


instance PrettyWithLoc (Token ",") where
  prettyWithLoc (Token l r) = DocWithLoc (pretty $ show TokComma) l r

instance PrettyWithLoc (Token ":") where
  prettyWithLoc (Token l r) = DocWithLoc (pretty $ show TokColon) l r

instance PrettyWithLoc (Token ":=") where
  prettyWithLoc (Token l r) = DocWithLoc (pretty $ show TokAssign) l r

instance PrettyWithLoc (Token "[!") where
  prettyWithLoc (Token l r) = DocWithLoc (pretty $ show TokSpecOpen) l r

instance PrettyWithLoc (Token "!]") where
  prettyWithLoc (Token l r) = DocWithLoc (pretty $ show TokSpecClose) l r

instance PrettyWithLoc (Token "(") where
  prettyWithLoc (Token l r) = DocWithLoc (pretty $ show TokParenOpen) l r

instance PrettyWithLoc (Token ")") where
  prettyWithLoc (Token l r) = DocWithLoc (pretty $ show TokParenClose) l r

instance PrettyWithLoc (Token "[") where
  prettyWithLoc (Token l r) = DocWithLoc (pretty $ show TokBracketOpen) l r

instance PrettyWithLoc (Token "]") where
  prettyWithLoc (Token l r) = DocWithLoc (pretty $ show TokBracketClose) l r

instance PrettyWithLoc (Token "{") where
  prettyWithLoc (Token l r) = DocWithLoc (pretty $ show TokBraceOpen) l r

instance PrettyWithLoc (Token "}") where
  prettyWithLoc (Token l r) = DocWithLoc (pretty $ show TokBraceClose) l r

instance PrettyWithLoc (Token "<|") where
  prettyWithLoc (Token l r) = DocWithLoc (pretty $ show TokQuantOpen) l r

instance PrettyWithLoc (Token "|>") where
  prettyWithLoc (Token l r) = DocWithLoc (pretty $ show TokQuantClose) l r

instance PrettyWithLoc (Token "⟨") where
  prettyWithLoc (Token l r) = DocWithLoc (pretty $ show TokQuantOpenU) l r

instance PrettyWithLoc (Token "⟩") where
  prettyWithLoc (Token l r) = DocWithLoc (pretty $ show TokQuantCloseU) l r

-- instance PrettyWithLoc (Token "{-") where
--   prettyWithLoc (Token l r) = DocWithLoc (pretty $ show TokProofOpen) l r

-- instance PrettyWithLoc (Token "-}") where
--   prettyWithLoc (Token l r) = DocWithLoc (pretty $ show TokProofClose) l r

instance PrettyWithLoc (Token "{:") where
  prettyWithLoc (Token l r) = DocWithLoc (pretty $ show TokDeclOpen) l r

instance PrettyWithLoc (Token ":}") where
  prettyWithLoc (Token l r) = DocWithLoc (pretty $ show TokDeclClose) l r

instance PrettyWithLoc (Token "|[") where
  prettyWithLoc (Token l r) = DocWithLoc (pretty $ show TokBlockOpen) l r

instance PrettyWithLoc (Token "]|") where
  prettyWithLoc (Token l r) = DocWithLoc (pretty $ show TokBlockClose) l r

instance PrettyWithLoc (Token "_") where
  prettyWithLoc (Token l r) = DocWithLoc (pretty $ show TokUnderscore) l r

--------------------------------------------------------------------------------

-- | Program
instance Pretty Program where
  pretty = toDoc . prettyWithLoc

instance PrettyWithLoc Program where
  prettyWithLoc (Program decls stmts) =
    prettyWithLoc decls <> prettyWithLoc stmts

--------------------------------------------------------------------------------

-- | Definition

instance Pretty DefinitionBlock where
  pretty = toDoc . prettyWithLoc

instance PrettyWithLoc DefinitionBlock where
  prettyWithLoc (DefinitionBlock l decls r) =
    prettyWithLoc l <> prettyWithLoc decls <> prettyWithLoc r

instance Pretty Definition where
  pretty = toDoc . prettyWithLoc

instance PrettyWithLoc Definition where
  prettyWithLoc (TypeDefn dat name binders eq qdcons) =
    prettyWithLoc dat
      <> prettyWithLoc name
      <> prettyWithLoc binders
      <> prettyWithLoc eq
      <> prettyWithLoc qdcons
  prettyWithLoc (FuncDefnSig base prop) =
    prettyWithLoc base <> maybe Empty prettyWithLoc prop
  prettyWithLoc (FuncDefn n args e b) =
    prettyWithLoc n <> prettyWithLoc args <> prettyWithLoc e <> prettyWithLoc b

--------------------------------------------------------------------------------

-- | Declaration
instance Pretty DeclBase where
  pretty = toDoc . prettyWithLoc

instance PrettyWithLoc DeclBase where
  prettyWithLoc (DeclBase names colon t) =
    prettyWithLoc names <> prettyWithLoc colon <> prettyWithLoc t

instance Pretty DeclProp where
  pretty = toDoc . prettyWithLoc

instance PrettyWithLoc DeclProp where
  prettyWithLoc (DeclProp l p r) =
    prettyWithLoc l <> prettyWithLoc p <> prettyWithLoc r

instance Pretty DeclType where
  pretty = toDoc . prettyWithLoc

instance PrettyWithLoc DeclType where
  prettyWithLoc (DeclType decl prop) =
    prettyWithLoc decl <> maybe Empty prettyWithLoc prop

instance Pretty Declaration where
  pretty = toDoc . prettyWithLoc

instance PrettyWithLoc Declaration where
  prettyWithLoc (ConstDecl con decl) = prettyWithLoc con <> prettyWithLoc decl
  prettyWithLoc (VarDecl   v   decl) = prettyWithLoc v <> prettyWithLoc decl

instance Pretty TypeDefnCtor where
  pretty = toDoc . prettyWithLoc

instance PrettyWithLoc TypeDefnCtor where
  prettyWithLoc (TypeDefnCtor n ts) = prettyWithLoc n <> prettyWithLoc ts

--------------------------------------------------------------------------------
-- | Literals
instance Pretty Lit where
  pretty = toDoc . prettyWithLoc

instance PrettyWithLoc Lit where
  prettyWithLoc (LitBool True  l) = fromDoc (locOf l) (pretty $ show TokTrue)
  prettyWithLoc (LitBool False l) = fromDoc (locOf l) (pretty $ show TokFalse)
  prettyWithLoc (LitInt  n     l) = fromDoc (locOf l) (pretty n)
  prettyWithLoc (LitChar c     l) = fromDoc (locOf l) ("'" <> pretty [c] <> "'")

--------------------------------------------------------------------------------

-- | Stmt
instance Pretty Stmt where
  pretty = toDoc . prettyWithLoc

instance PrettyWithLoc Stmt where
  prettyWithLoc (Skip  l) = fromDoc (locOf l) (pretty $ show TokSkip)
  prettyWithLoc (Abort l) = fromDoc (locOf l) (pretty $ show TokAbort)
  prettyWithLoc (Assign xs a es) =
    prettyWithLoc xs <> prettyWithLoc a <> prettyWithLoc es
  prettyWithLoc (AAssign x l i r a e) =
    prettyWithLoc x
      <> prettyWithLoc l
      <> prettyWithLoc i
      <> prettyWithLoc r
      <> prettyWithLoc a
      <> prettyWithLoc e
  prettyWithLoc (Assert l p r) =
    prettyWithLoc l <> prettyWithLoc p <> prettyWithLoc r
  prettyWithLoc (LoopInvariant l p c b bnd d r) =
    prettyWithLoc l
      <> prettyWithLoc p
      <> prettyWithLoc c
      <> prettyWithLoc b
      <> prettyWithLoc bnd
      <> prettyWithLoc d
      <> prettyWithLoc r
  prettyWithLoc (Do l gdCmds r) =
    prettyWithLoc l <> prettyWithLoc gdCmds <> prettyWithLoc r
  prettyWithLoc (If l gdCmds r) =
    prettyWithLoc l <> prettyWithLoc gdCmds <> prettyWithLoc r
  prettyWithLoc (SpecQM l) = fromDoc (locOf l) (pretty $ show TokQM)
  prettyWithLoc (Spec l s r) =
    prettyWithLoc l
      <> prettyWithLoc (map (fmap show) s)
      <> prettyWithLoc r
      -- where 
        -- don't show Tokens like <newline> or <indent>
        -- show' Tok = case Tok of 
        --   TokNewlineAndWhitespace _ -> ""
        --   TokNewlineAndWhitespaceAndBar _ -> ""
        --   TokIndent            -> ""
        --   TokDedent            -> ""
        --   TokNewline           -> ""
        --   _ -> show Tok 
  prettyWithLoc (Proof _ _ whole r) =
    fromDoc (locOf r) (pretty whole)
  prettyWithLoc (Alloc p a n l es r) =
    prettyWithLoc p
      <> prettyWithLoc a
      <> prettyWithLoc n
      <> prettyWithLoc l
      <> prettyWithLoc es
      <> prettyWithLoc r
  prettyWithLoc (HLookup x a s e) =
    prettyWithLoc x <> prettyWithLoc a <> prettyWithLoc s <> prettyWithLoc e
  prettyWithLoc (HMutate s e1 a e2) =
    prettyWithLoc s <> prettyWithLoc e1 <> prettyWithLoc a <> prettyWithLoc e2
  prettyWithLoc (Dispose l e) = prettyWithLoc l <> prettyWithLoc e
  prettyWithLoc (Block l p r) =
    prettyWithLoc l <> prettyWithLoc p <> prettyWithLoc r

instance Pretty GdCmd where
  pretty = toDoc . prettyWithLoc

instance PrettyWithLoc GdCmd where
  prettyWithLoc (GdCmd guard a body) =
    prettyWithLoc guard <> prettyWithLoc a <> prettyWithLoc body

-- instance Pretty ProofAnchor where
--   pretty = toDoc . prettyWithLoc

-- instance PrettyWithLoc ProofAnchor where
--   prettyWithLoc (ProofAnchor hash range) =
--     fromDoc (locOf range) (pretty (show TokHash) <> pretty hash)

-- instance PrettyWithLoc TextContents where
--   prettyWithLoc (TextContents text range) =
--     fromDoc (locOf range) (pretty text)

--------------------------------------------------------------------------------

-- | Expr
instance Pretty Expr where
  pretty = toDoc . prettyWithLoc

instance PrettyWithLoc Expr where
  prettyWithLoc expr = case handleExpr expr of
    Expect   _ -> mempty
    Complete s -> s

handleExpr :: Expr -> Variadic Expr (DocWithLoc ann)
handleExpr (Paren l x r) =
  return $ prettyWithLoc l <> prettyWithLoc x <> prettyWithLoc r
handleExpr (Var   x) = return $ prettyWithLoc x
handleExpr (Const x) = return $ prettyWithLoc x
handleExpr (Lit   x) = return $ prettyWithLoc x
handleExpr (Op    x) = handleArithOp x
handleExpr (Chain c) = handleChain c
handleExpr (Arr arr l i r) =
  return
    $  prettyWithLoc arr
    <> prettyWithLoc l
    <> prettyWithLoc i
    <> prettyWithLoc r
handleExpr (App p q) = case handleExpr p of
  Expect   f -> f q
  Complete s -> do
    t <- handleExpr q
    return $ s <> t
handleExpr (Quant open op xs m r n t close) =
  return
    $  prettyWithLoc open
    <> prettyWithLoc op
    <> prettyWithLoc xs
    <> prettyWithLoc m
    <> prettyWithLoc r
    <> prettyWithLoc n
    <> prettyWithLoc t
    <> prettyWithLoc close
handleExpr (Case a expr b cases) =
  return
    $  prettyWithLoc a
    <> prettyWithLoc expr
    <> prettyWithLoc b
    <> prettyWithLoc cases

handleArithOp :: ArithOp -> Variadic Expr (DocWithLoc ann)
handleArithOp op = case classify (ArithOp op) of -- TODO: rewrite `classify` to only handle `ArithOp`s.
  (Infix, _) -> do
    p <- var
    q <- var
    return $ prettyWithLoc p <> prettyWithLoc op <> prettyWithLoc q
  (InfixL, _) -> do
    p <- var
    q <- var
    return $ prettyWithLoc p <> prettyWithLoc op <> prettyWithLoc q
  (InfixR, _) -> do
    p <- var
    q <- var
    return $ prettyWithLoc p <> prettyWithLoc op <> prettyWithLoc q
  (Prefix, _) -> do
    p <- var
    return $ prettyWithLoc op <> prettyWithLoc p
  (Postfix, _) -> do
    p <- var
    return $ prettyWithLoc p <> prettyWithLoc op

handleChain :: Chain -> Variadic Expr (DocWithLoc ann)
handleChain chain = case chain of
  Pure expr -> handleExpr expr
  More ch op expr -> do
    ch' <- handleChain ch
    return $ ch' <> prettyWithLoc op <> prettyWithLoc expr


showWithParentheses :: Expr -> String
showWithParentheses expr = case handleExpr' expr of
  Expect _ -> error "strange case in printWithParenses"
  Complete s -> s
  where
    handleExpr' :: Expr -> Variadic Expr String
    handleExpr' (Paren _ x _) = handleExpr' x
    handleExpr' (Var   x) = return $ unpack $ nameToText x
    handleExpr' (Const x) = return $ unpack $ nameToText x
    handleExpr' (Lit   x) = case x of
      LitInt n _ -> return $ show n
      LitBool b _ -> return $ show b
      LitChar c _ -> return $ show c
    handleExpr' (Op    x) = handleArithOp' x
    handleExpr' (Chain c) = handleChain' c
    handleExpr' (Arr arr _ i _) = do
      arrs <- handleExpr' arr
      inds   <- handleExpr' i
      return $  arrs <> "[" <> inds <> "]"
    handleExpr' (App p q) = case handleExpr' p of
      Expect   f -> f q
      Complete s -> do
        t <- handleExpr' q
        return $ "(" <> s <> " " <> t <> ")"
    handleExpr' q@Quant {} =
      return $ show $ pretty q
    handleExpr' c@Case {} =
      return $ show $ pretty c

    handleArithOp' :: ArithOp -> Variadic Expr String
    handleArithOp' op = case classify (ArithOp op) of
      (Infix, _) -> do
        p <- var
        q <- var
        ps <- handleExpr' p
        qs <- handleExpr' q
        return $ "(" <> ps <> show (pretty op) <> qs <> ")"
      (InfixL, _) -> do
        p <- var
        q <- var
        ps <- handleExpr' p
        qs <- handleExpr' q
        return $ "(" <> ps <> show (pretty op) <> qs <> ")"
      (InfixR, _) -> do
        p <- var
        q <- var
        ps <- handleExpr' p
        qs <- handleExpr' q
        return $ "(" <> ps <> show (pretty op) <> qs <> ")"
      (Prefix, _) -> do
        p <- var
        ps <- handleExpr' p
        return $ "(" <> show (pretty op) <> ps <> ")"
      (Postfix, _) -> do
        p <- var
        ps <- handleExpr' p
        return $ "(" <> ps <> show (pretty op) <> ")"

    handleChain' :: Chain -> Variadic Expr String
    handleChain' chain = case chain of
      Pure expr' -> handleExpr' expr'
      More ch op ex -> do
        ch' <- handleChain' ch
        ex' <- handleExpr' ex
        return $ ch' <> show (pretty op) <> ex'


--------------------------------------------------------------------------------
-- | Pattern

instance PrettyWithLoc CaseClause where
  prettyWithLoc (CaseClause a b c) =
    prettyWithLoc a <> prettyWithLoc b <> prettyWithLoc c

instance Pretty Pattern where
  pretty = toDoc . prettyWithLoc

instance PrettyWithLoc Pattern where
  prettyWithLoc patt = case patt of
    PattLit a           -> prettyWithLoc a
    PattParen a b c     -> prettyWithLoc a <> prettyWithLoc b <> prettyWithLoc c
    PattBinder   a      -> prettyWithLoc a
    PattWildcard a      -> prettyWithLoc a
    PattConstructor a b -> prettyWithLoc a <> prettyWithLoc b

--------------------------------------------------------------------------------

-- | Type
instance Pretty Type where
  pretty = toDoc . prettyWithLoc

instance PrettyWithLoc Type where -- TODO: Prettyprint infix type operators correctly.
  prettyWithLoc (TParen l t r) =
    prettyWithLoc l <> prettyWithLoc t <> prettyWithLoc r
  -- DocWithLoc "(" l l <> prettyWithLoc t <> DocWithLoc ")" m m
  prettyWithLoc (TBase (TInt  l)) = fromDoc (locOf l) (pretty ("Int"::String))
  prettyWithLoc (TBase (TBool l)) = fromDoc (locOf l) (pretty ("Bool"::String))
  prettyWithLoc (TBase (TChar l)) = fromDoc (locOf l) (pretty ("Char"::String))
  prettyWithLoc (TArray l a r b) =
    prettyWithLoc l <> prettyWithLoc a <> prettyWithLoc r <> prettyWithLoc b
  prettyWithLoc (TOp op   ) = prettyWithLoc op
  prettyWithLoc (TData d _) = prettyWithLoc d
  prettyWithLoc (TApp a b ) = prettyWithLoc a <> prettyWithLoc b
  prettyWithLoc (TMetaVar i _) = prettyWithLoc i

--------------------------------------------------------------------------------

-- | Endpoint & Interval
instance PrettyWithLoc EndpointOpen where
  prettyWithLoc (IncludingOpening l e) = prettyWithLoc l <> prettyWithLoc e
  prettyWithLoc (ExcludingOpening l e) = prettyWithLoc l <> prettyWithLoc e

instance PrettyWithLoc EndpointClose where
  prettyWithLoc (IncludingClosing e l) = prettyWithLoc e <> prettyWithLoc l
  prettyWithLoc (ExcludingClosing e l) = prettyWithLoc e <> prettyWithLoc l

instance PrettyWithLoc Interval where
  prettyWithLoc (Interval a b c) =
    prettyWithLoc a <> prettyWithLoc b <> prettyWithLoc c
