{-# LANGUAGE OverloadedStrings #-}

{-# LANGUAGE TypeSynonymInstances #-}
module Pretty.Error where

import           Data.Loc
import           Data.Text.Prettyprint.Doc
import           Error
import qualified GCL.Scope                     as Scope
import           GCL.Type                       ( TypeError(..) )
import           GCL.WP.Type                    ( StructError(..)
                                                , StructWarning(..)
                                                )
import           Prelude                 hiding ( Ordering(..) )
import           Pretty.Abstract                ( )
import           Pretty.Predicate               ( )
import           Pretty.Util                    ( )


-- | Error
instance Pretty Error where
  pretty (SyntacticError (pos, msg)) =
    "Syntactic Error" <+> pretty (displayPos pos) <+> pretty msg
  pretty (ScopeError err) =
    "Scope Error" <+> pretty (locOf err) <> line <> pretty err
  pretty (TypeError err) =
    "Type Error" <+> pretty (locOf err) <> line <> pretty err
  pretty (StructError err) =
    "Struct Error" <+> pretty (locOf err) <> line <> pretty err
  pretty (CannotReadFile path) = "CannotReadFile" <+> pretty path
  pretty (Others         msg ) = "Others" <+> pretty msg

instance Pretty StructWarning where
  pretty (MissingBound loc) = "Missing Bound" <+> pretty loc
  pretty (ExcessBound  loc) = "Excess Bound" <+> pretty loc

instance Pretty StructError where
  pretty (MissingAssertion     loc) = "Missing Assertion" <+> pretty loc
  pretty (MissingPostcondition loc) = "Missing Postcondition" <+> pretty loc
  pretty (MultiDimArrayAsgnNotImp loc) =
    "Assignment to Multi-Dimensional Array" <+> pretty loc

instance Pretty Scope.ScopeError where
  pretty (Scope.NotInScope name) =
    "The identifier" <+> pretty name <+> "is not in scope"
  pretty (Scope.DuplicatedIdentifiers ns) =
    "Duplicated identifiers:" <+> hsep (punctuate ", " (map pretty ns))
  pretty (Scope.RedundantNames ns) =
    "The identifiers"
      <+> hsep (punctuate ", " (map pretty ns))
      <+> "are redundant"
  pretty (Scope.RedundantPatterns patts) =
    "The patterns"
      <+> hsep (punctuate ", " (map pretty patts))
      <+> "are redundant"
  pretty (Scope.RedundantExprs exprs) =
    "The expressions"
      <+> hsep (punctuate ", " (map pretty exprs))
      <+> "are redundant"

instance Pretty TypeError where
  pretty (NotInScope name _) =
    "The definition" <+> pretty name <+> "is not in scope"
  pretty (UnifyFailed a b _) =
    "Cannot unify:" <+> pretty a <+> "with" <+> pretty b
  pretty (RecursiveType v a _) =
    "Recursive type variable: " <+> pretty v <+> "in" <+> pretty a
  pretty (NotFunction a _) =
    "The type" <+> pretty a <+> "is not a function type"
  pretty (NotArray a _) = "The type" <+> pretty a <+> "is not an array type"
  pretty (NotEnoughExprsInAssigment vars _) =
    "Not Enough Expressions:"
      <+> "Variables"
      <+> pretty vars
      <+> "do not have corresponing expressions in the assigment"
  pretty (TooManyExprsInAssigment exprs _) =
    "Too Many Expressions: "
      <+> "Expressions"
      <+> pretty exprs
      <+> "do not have corresponing variables in the assigment"
  pretty (AssignToConst n _) =
    "Assginment to Constant Declaration"
      <+> "Declaration"
      <+> pretty n
      <+> "is a constant, not a variable"
  pretty (AssignToLet n _) =
    "Assginment to Let Declaration"
      <+> "Declaration"
      <+> pretty n
      <+> "is a let binding, not a variable"
  pretty (UndefinedType n _) =
    "Undefined Type: " <+> "Type" <+> pretty n <+> "is undefined"
  pretty (DuplicatedIdentifier n _) =
    "Duplicated Identifier: " <+> "Identifier" <+> pretty n <+> "is duplicated"
