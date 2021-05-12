{-# LANGUAGE OverloadedStrings #-}

{-# LANGUAGE TypeSynonymInstances #-}
module Pretty.Error where

import Data.Loc
import Data.Text.Prettyprint.Doc
import Pretty.Abstract ()
import Pretty.Predicate ()
import Pretty.Util ()
import Prelude hiding (Ordering (..))
import Error
import GCL.WP (StructWarning(..), StructError(..))
import GCL.Type (TypeError(..))


-- | Error
instance Pretty Error where
  pretty (SyntacticError err) = "Syntactic Error" <+> pretty err
  pretty (TypeError err) =
    "Type Error" <+> pretty (locOf err) <> line <> pretty err
  pretty (StructError err) =
    "Struct Error" <+> pretty (locOf err) <> line <> pretty err
  pretty (CannotReadFile path) = "CannotReadFile" <+> pretty path
  pretty (Others msg) = "Others" <+> pretty msg

instance Pretty StructWarning where
  pretty (MissingBound loc) = "Missing Bound" <+> pretty loc
  pretty (ExcessBound loc) = "Excess Bound" <+> pretty loc

instance Pretty StructError where
  pretty (MissingAssertion loc) = "Missing Assertion" <+> pretty loc
  pretty (MissingPostcondition loc) = "Missing Postcondition" <+> pretty loc

instance Pretty TypeError where
  pretty (NotInScope name _) =
    "The definition" <+> pretty name <+> "is not in scope"
  pretty (UnifyFailed a b _) =
    "Cannot unify:" <+> pretty a <+> "with" <+> pretty b
  pretty (RecursiveType v a _) =
    "Recursive type variable: " <+> pretty v <+> "in" <+> pretty a
  pretty (NotFunction a _) =
    "The type" <+> pretty a <+> "is not a function type"