{-# LANGUAGE OverloadedStrings #-}

module Pretty.Concrete where

import Data.Text.Prettyprint.Doc
import Pretty.Abstract ()
import Pretty.Util
import Syntax.Concrete hiding (GT)
import Syntax.Location
import Data.Loc
import Pretty.Variadic
import Syntax.Abstract (Fixity(..), classify)

--------------------------------------------------------------------------------

-- | Program
instance Pretty Program where
  pretty (Program decls _ _ stmts _) =
    vsep (map pretty decls ++ map pretty stmts)

--------------------------------------------------------------------------------

-- | Declaration
instance Pretty Declaration where
  pretty (ConstDecl names t Nothing _) =
    "con" <+> hsep (punctuate comma (map pretty names)) <+> ":" <+> pretty t
  pretty (ConstDecl names t (Just p) _) =
    "con"
      <+> hsep (punctuate comma (map pretty names))
      <+> ":"
      <+> pretty t
      <+> braces (pretty p)
  pretty (VarDecl names t Nothing _) =
    "var" <+> hsep (punctuate comma (map pretty names)) <+> ":" <+> pretty t
  pretty (VarDecl names t (Just p) _) =
    "var"
      <+> hsep (punctuate comma (map pretty names))
      <+> ":"
      <+> pretty t
      <+> braces (pretty p)
  pretty (LetDecl name args expr _) =
    "let" <+> pretty name <+> hsep (map pretty args) <+> "=" <+> pretty expr

-- vsep (map pretty decls ++ map pretty stmts)

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
    hsep (punctuate comma (map (pretty . depart) xs)) <+> ":="
      <+> hsep
        (punctuate comma (map (pretty . depart) es))
  pretty (Assert p _) = lbrace <+> pretty p <+> rbrace
  pretty (LoopInvariant p bnd _) =
    lbrace <+> pretty p <+> ", bnd:" <+> pretty bnd <+> rbrace
  pretty (Do gdCmds _) = "do" <+> align (encloseSep mempty mempty " | " (map pretty gdCmds)) <> line <> "od"
  pretty (If gdCmds _) = "if" <+> align (encloseSep mempty mempty " | " (map pretty gdCmds)) <> line <> "fi"
  pretty (SpecQM _) = "?"
  pretty (Spec _) = "{!  !}"
  pretty (Proof _) = "{-  -}"

instance Pretty GdCmd where
  pretty (GdCmd guard body _) = pretty guard <+> "->" <+> align (vsep (map pretty body))

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
    Expect   _ -> mempty
    Complete s -> s

handleExpr :: Int -> Expr -> Variadic Expr (DocWithLoc ann)
handleExpr _ (Var   x l) = return $ fromPretty x l
handleExpr _ (Const x l) = return $ fromPretty x l
handleExpr _ (Lit   x l) = return $ fromPretty x l
handleExpr n (Op    x l) = handleOp n x l
handleExpr n (App p q _) = case handleExpr n p of
  Expect   f -> f q
  Complete s -> do
    t <- handleExpr n q
    -- see if the second argument is an application, apply parenthesis when needed
    return $ case q of
      App {} -> s <> parensIf' n 0 t
      _      -> s <> t
handleExpr _ (Lam p q l) = return $ overrideLoc l $ "λ " <> fromPretty p noLoc <> " → " <> prettyWithLoc q
handleExpr _ (Hole l) = return $ fromPretty'' (prettyHole l) l
handleExpr _ (Quant op xs r t l) = return "Quant"
handleExpr _ (Subst x _) = return "Subst"
  -- return
  --   $   "⟨"
  --   <>  fromPretty (prettyQuantOp op) (locOf op)
  --   <>  mconcat (map prettyWithLoc xs)
  --   <> " : "
  --   <+> pretty r
  --   <+> ":"
  --   <+> pretty t
  --   <>  "⟩"



handleOp :: Int -> Op -> Loc -> Variadic Expr (DocWithLoc ann)
handleOp n op loc = case classify op of
  Infix m -> do
    p <- var
    q <- var
    return  $ parensIf' n m
            $  prettyPrecWithLoc (succ m) p
            <> fromPretty op loc
            <> prettyPrecWithLoc (succ m) q
  InfixL m -> do
    p <- var
    q <- var
    return  $ parensIf' n m
            $  prettyPrecWithLoc m p
            <> fromPretty op loc
            <> prettyPrecWithLoc (succ m) q
  InfixR m -> do
    p <- var
    q <- var
    return  $ parensIf' n m
            $  prettyPrecWithLoc (succ m) p
            <> fromPretty op loc
            <> prettyPrecWithLoc m q
  Prefix m -> do
    p <- var
    return $ parensIf' n m $ fromPretty op loc <> prettyPrecWithLoc m p
  Postfix m -> do
    p <- var
    return $ parensIf' n m $ prettyPrecWithLoc m p <> fromPretty op loc

prettyHole :: Loc -> Doc ann
prettyHole loc = case loc of 
    NoLoc -> "{!!}"
    Loc start end -> "{!" <> fillGap (translate 2 start) (translate (-2) end) <> "!}"

-- instance Pretty Expr where
-- pretty = pretty . depart 
-- pretty (Lit x _) = pretty x
-- pretty (Var x _) = pretty x 
-- pretty (Const x _) = pretty x 
-- pretty (Op x _) = pretty x 
-- pretty (App x y _) = prettyWithLocs [(pretty x, locOf x), (pretty y, locOf y)]
-- pretty (Lam x y _) = prettyWithLocs [(pretty x, locOf x), (pretty y, locOf y)]
-- pretty (Hole loc) = case loc of 
--   NoLoc -> "{!!}"
--   Loc (Pos p1 l1 c1 o1) (Pos p2 l2 c2 o2) -> 
--     let start' = Pos p1 l1 (c1 + 2) (o1 + 2)
--         end' = Pos p2 l2 (c2 - 2) (o1 - 2)
--     in  "{!" <> fillGap start' end' <> "!}"
-- pretty (Quant a b c d) = 
-- | Quant Expr [Name] Expr Expr Loc
-- | Subst Expr Subst -- internal. Location not necessary?

--------------------------------------------------------------------------------

-- | Type

instance Pretty Interval where
  pretty = pretty . depart

instance Pretty Type where
  pretty = pretty . depart
