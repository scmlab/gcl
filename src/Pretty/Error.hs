{-# LANGUAGE OverloadedStrings #-}

{-# LANGUAGE TypeSynonymInstances #-}
module Pretty.Error where

import           Data.Foldable                  ( toList )
import           Data.Loc
import           Prettyprinter
import           Error
import           GCL.Type                     ( TypeError(..) )
import           GCL.WP.Types                   ( StructError(..)
                                                , StructWarning(..)
                                                )
import           Prelude                 hiding ( Ordering(..) )
import           Pretty.Abstract                ( )
import           Pretty.Predicate               ( )
import           Pretty.Util                    ( )
import           Syntax.Parser.Error           ( ParseError(..) )

-- | Error
instance Pretty Error where
  pretty (ParseError err) = "Parse Error" <+> line <> pretty err
  pretty (TypeError err) =
    "Type Error" <+> pretty (locOf err) <> line <> pretty err
  pretty (StructError err) =
    "Struct Error" <+> pretty (locOf err) <> line <> pretty err
  pretty (CannotReadFile path) = "CannotReadFile" <+> pretty path
  pretty (Others         title msg loc) = "Others" <+> pretty title <+> pretty msg <+> pretty (displayLoc loc)

instance Pretty ParseError where
  pretty (LexicalError   pos  ) = "Lexical Error" <+> pretty (displayPos pos)
  pretty (SyntacticError pairs _) = "Parse Error" <+> vsep
    (map (\(loc, msg) -> pretty (displayLoc loc) <+> pretty msg) $ toList pairs)
    -- the second argument was parsing log, used for debugging

instance Pretty StructWarning where
  pretty (MissingBound loc) = "Missing Bound" <+> pretty loc

instance Pretty StructError where
  pretty (MissingAssertion     loc) = "Missing Assertion" <+> pretty loc
  pretty (MissingPostcondition loc) = "Missing Postcondition" <+> pretty loc
  pretty (MultiDimArrayAsgnNotImp loc) =
    "Assignment to Multi-Dimensional Array" <+> pretty loc
  pretty (LocalVarExceedScope loc) =
    "Local Variable(s) Exceeded Scope" <+> pretty loc

instance Pretty TypeError where
  pretty (NotInScope name) =
    "The definition" <+> pretty name <+> "is not in scope"
  pretty (UnifyFailed a b _) =
    "Cannot unify:" <+> pretty a <+> "with" <+> pretty b
  pretty (KindUnifyFailed a b _) =
    "Cannot unify:" <+> pretty a <+> "with" <+> pretty b
  pretty (RecursiveType v a _) =
    "Recursive type variable: " <+> pretty v <+> "in" <+> pretty a
  pretty (AssignToConst n) =
    "The constant identifier: " <+> pretty n <+> "cannot be assigned"
  pretty (UndefinedType n) =
    "Undefined Type: " <+> "Type" <+> pretty n <+> "is undefined"
  pretty (DuplicatedIdentifiers ns) =
    "The identifiers:" <+> pretty ns <+> "are duplicated"
  pretty (RedundantNames ns) =
    "The names: " <+> pretty ns <+> "are redundant"
  pretty (RedundantExprs exprs) =
    "The exprs: " <+> pretty exprs <+> "are redundant"
  pretty (MissingArguments ns) =
    "The arguments: " <+> pretty ns <+> "are missing"
  pretty (PatternArityMismatch expected actual _) =
    "Expect" <+> pretty expected <+> "arguments but found" <+> pretty actual
