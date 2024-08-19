{-# LANGUAGE OverloadedStrings #-}

module Pretty.Typed
  () where

import           Prelude                 hiding ( Ordering(..) )
import           Pretty.Common                  ( )
import           Pretty.Util
import           Prettyprinter
import           Render.Error                   ( )
import           Render.Predicate               ( )
import           Render.Syntax.Common           ( )
import           Syntax.Typed
import           Pretty.Abstract                ( )
import Render.Class (PrecContext(NoContext))

--------------------------------------------------------------------------------

-- | Program
instance Pretty Program where
  pretty (Program _ decls props stmts _) =
    vsep $ map pretty decls ++ map pretty props ++ map pretty stmts

--------------------------------------------------------------------------------

-- | Declaration
instance Pretty Declaration where
  pretty (ConstDecl names t Nothing _) =
    "con " <> hsep (punctuate ", " (map pretty names)) <> ": " <> pretty t
  pretty (ConstDecl names t (Just p) _) =
    "con "
      <> hsep (punctuate ", " (map pretty names))
      <> ": "
      <> pretty t
      <> "{ "
      <> pretty p
      <> " }"
  pretty (VarDecl names t Nothing _) =
    "var " <> hsep (punctuate ", " (map pretty names)) <> ": " <> pretty t
  pretty (VarDecl names t (Just p) _) =
    "var "
      <> hsep (punctuate ", " (map pretty names))
      <> ": "
      <> pretty t
      <> "{ "
      <> pretty p
      <> " }"

-- TODO: These comments are here to remind us that these are not yet implemented.
instance Pretty Definition where
  pretty (TypeDefn name binders qdcons _) =
    "data " <> pretty name <+> hsep (map pretty binders) <> "= " <> hsep
      (punctuate "| " (map pretty qdcons))
  pretty (FuncDefnSig name typ Nothing _) = pretty name <> ": " -- <> pretty typ
  pretty (FuncDefnSig name typ (Just prop) _) =
    pretty name <> ": " <> {- pretty typ <> -} "{ " <> pretty prop <> " }"
  pretty (FuncDefn name expr) = pretty name <+> " = " <+> pretty expr

instance Pretty TypeDefnCtor where
  pretty (TypeDefnCtor cn ts) = pretty cn <+> hsep (pretty <$> ts)

instance Pretty KindedType where -- FIXME: Implement this.

--------------------------------------------------------------------------------

-- | Stmt
instance Pretty Stmt where
  pretty (Skip  _) = "skip"
  pretty (Abort _) = "abort"
  pretty (Assign xs es _) =
    hsep (punctuate ", " (map pretty xs)) <> ":= " <> hsep
      (punctuate ", " (map pretty es))
  pretty (AAssign x i e _) =
    pretty x <> "[" <> pretty i <> "]" <> ":=" <> pretty e
  pretty (Assert p _) = "{ " <> pretty p <> " }"
  pretty (LoopInvariant p bnd _) =
    "{ " <> pretty p <> " , bnd: " <> pretty bnd <> " }"
  pretty (Do gdCmds _) =
    "do" <> line <> vsep (map (\x -> " |" <+> pretty x <> line) gdCmds) <> "od"
  pretty (If gdCmds _) =
    "if" <> line <> vsep (map (\x -> " |" <+> pretty x <> line) gdCmds) <> "fi"
  pretty (Spec content _ _) = "[!" <> pretty content <> "!]"
  pretty (Proof anchor contents _) =
    -- "{-" <> vsep (map (\x -> pretty x <> line) anchors) <> "-}"
    "{- #" <> pretty anchor <> line <> pretty contents <> line <> "-}"
  pretty (Alloc x es _) =
    pretty x
      <+> ":="
      <+> "new ("
      <+> hsep (punctuate ", " (map pretty es))
      <+> ")"
  pretty (HLookup x  e  _) = pretty x <+> ":=" <+> pretty e <> "*"
  pretty (HMutate e1 e2 _) = pretty e1 <> "*" <+> ":=" <+> pretty e2
  pretty (Dispose e _    ) = "free" <+> pretty e
  pretty (Block   p _    ) = "|[" <+> pretty p <+> "]|"

instance Pretty GdCmd where
  pretty (GdCmd guard body _) =
    pretty guard <+> "->" <+> vsep (map pretty body)

-- instance Pretty ProofAnchor where
--   pretty (ProofAnchor hash _) = "#" <+> pretty hash

-- instance Pretty TextContents where
--   pretty (TextContents text _) = pretty text

--------------------------------------------------------------------------------

-- | Expr
instance Pretty Expr where
  pretty = prettyPrec NoContext

instance PrettyPrec Expr where
  prettyPrec = fromRenderPrec

-- instance Pretty Pattern where
--   pretty = prettyPrec NoContext
--
-- instance PrettyPrec Pattern where
--   prettyPrec = fromRenderPrec

--------------------------------------------------------------------------------
