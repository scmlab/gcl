{-# LANGUAGE OverloadedStrings #-}

module Pretty.Abstract () where

import Data.Text.Prettyprint.Doc
import Pretty.Common ()
import Pretty.Util
import Syntax.Abstract
import Prelude hiding (Ordering (..))

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

-- | Stmt
instance Pretty Stmt where
  pretty (Skip _) = "skip"
  pretty (Abort _) = "abort"
  pretty (Assign xs es _) =
    hsep (punctuate ", " (map pretty xs))
      <> ":= "
      <> hsep (punctuate ", " (map pretty es))
  pretty (AAssign x i e _) =
    pretty x <> "[" <> pretty i <> "]" <> ":=" <> pretty e
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
  pretty = fromRender

--------------------------------------------------------------------------------

-- | Expr
instance Pretty Expr where
  pretty = prettyPrec 0

instance PrettyPrec Expr where
  prettyPrec = fromRenderPrec

--------------------------------------------------------------------------------

-- | Type
instance Pretty Type where
  pretty = fromRender

--------------------------------------------------------------------------------

-- | Interval
instance Pretty Interval where
  pretty = fromRender
