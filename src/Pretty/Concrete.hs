{-# LANGUAGE OverloadedStrings #-}

module Pretty.Concrete where

import           Data.Text.Prettyprint.Doc
-- import Prelude hiding (Ordering(..))

import           Syntax.Location
import           Syntax.Concrete
import           Pretty.Abstract                ( )
import           Pretty.Util

--------------------------------------------------------------------------------
-- | Stmt

instance Pretty Stmt where
  pretty (Skip  _) = "skip"
  pretty (Abort _) = "abort"
  pretty (Assign xs es _) =
    hsep (punctuate comma (map (pretty . depart) xs)) <+> ":=" <+> hsep
      (punctuate comma (map (pretty . depart) es))
  pretty (Assert p _) = lbrace <+> pretty p <+> rbrace
  pretty (LoopInvariant p bnd _) =
    lbrace <+> pretty p <+> "bnd:" <+> pretty bnd <+> rbrace
  pretty (Do gdCmds _) =
    "do" <+> align (encloseSep mempty mempty " |" (map pretty gdCmds)) <+> "od"
  pretty (If gdCmds _) =
    "if" <+> align (encloseSep mempty mempty " |" (map pretty gdCmds)) <+> "fi"
  pretty (SpecQM _) = "?"
  pretty (Spec   _) = "{!  !}"

instance Pretty GdCmd where
  pretty (GdCmd guard body _) =
    pretty guard <+> "->" <+> vsep (map pretty body)

--------------------------------------------------------------------------------
-- | Expr

instance PrettyPrec Expr where
  prettyPrec n = prettyPrec n . depart

instance Pretty Expr where
  pretty = pretty . depart

--------------------------------------------------------------------------------
-- | Type

-- instance Pretty Endpoint where
--   pretty (Including e) = ""
instance Pretty Interval where
  pretty = pretty . depart

instance Pretty Type where
  pretty = pretty . depart
