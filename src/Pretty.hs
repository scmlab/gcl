{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}

module Pretty
  ( module Data.Text.Prettyprint.Doc,
  )
where

import Data.Loc
import Data.Text.Prettyprint.Doc
import Error
import GCL.Type (TypeError (..))
import GCL.WP (StructError (..))
import Pretty.Abstract ()
import Pretty.Concrete ()
import Pretty.Predicate ()
import Syntax.Parser.Lexer (LexicalError)
import Prelude hiding (Ordering (..))

--------------------------------------------------------------------------------

-- | Misc
instance (Pretty a, Pretty b) => Pretty (Either a b) where
  pretty (Left a) = "Error" <+> pretty a
  pretty (Right b) = pretty b

--------------------------------------------------------------------------------

-- | Error
instance Pretty Error where
  pretty (LexicalError err) = "Lexical Error" <+> pretty err
  pretty (SyntacticError errors) = "Syntactic Error" <+> prettyList errors
  pretty (TypeError err) =
    "Type Error" <+> pretty (locOf err) <> line <> pretty err
  pretty (StructError err) =
    "Struct Error" <+> pretty (locOf err) <> line <> pretty err
  pretty (CannotReadFile path) = "CannotReadFile" <+> pretty path
  pretty NotLoaded = "NotLoaded"

instance Pretty LexicalError where
  pretty = pretty . show

instance Pretty StructError where
  pretty (MissingAssertion loc) = "Missing Assertion" <+> pretty loc
  pretty (MissingBound loc) = "Missing Bound" <+> pretty loc
  pretty (ExcessBound loc) = "Excess Bound" <+> pretty loc
  pretty (MissingPostcondition loc) = "Missing Postcondition" <+> pretty loc
  pretty (DigHole loc) = "Dig Hole" <+> pretty loc

instance Pretty TypeError where
  pretty (NotInScope name _) =
    "The definition" <+> pretty name <+> "is not in scope"
  pretty (UnifyFailed a b _) =
    "Cannot unify:" <+> pretty a <+> "with" <+> pretty b
  pretty (RecursiveType v a _) =
    "Recursive type variable: " <+> pretty v <+> "in" <+> pretty a
  pretty (NotFunction a _) =
    "The type" <+> pretty a <+> "is not a function type"

-- --------------------------------------------------------------------------------
-- -- | Val
--
-- instance Pretty Val where
--   pretty = pretty . show
