{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE TypeSynonymInstances, FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Pretty where

import Data.Text.Prettyprint.Doc
import Prelude hiding (Ordering(..))
import Data.Loc

import Error
import Syntax.Parser.Lexer (LexicalError)
import GCL.WP (StructError(..))
import qualified GCL.WP2 as WP2
import GCL.Type (TypeError(..))

import Pretty.Abstract ()
import Pretty.GCL.WP ()
import Pretty.GCL.WP2 ()

--------------------------------------------------------------------------------
-- | Error

instance Pretty Error where
  pretty (LexicalError err) = "Lexical Error" <+> pretty err
  pretty (SyntacticError (loc, err)) = "Syntactic Error" <+> pretty loc <> line
    <> pretty err
  pretty (TypeError err) = "Type Error" <+> pretty (locOf err) <> line <> pretty err
  pretty (StructError err) = "Struct Error" <+> pretty (locOf err) <> line <> pretty err
  pretty (StructError2 err) = "Struct Error" <+> pretty (locOf err) <> line <> pretty err

instance Pretty LexicalError where
  pretty = pretty

instance Pretty StructError where
  pretty (MissingAssertion loc) = "Missing Assertion" <+> pretty loc
  pretty (MissingBound loc) = "Missing Bound" <+> pretty loc
  pretty (ExcessBound loc) = "Excess Bound" <+> pretty loc
  pretty (MissingPostcondition loc) = "Missing Postcondition" <+> pretty loc
  pretty (DigHole loc) = "Dig Hole" <+> pretty loc

instance Pretty WP2.StructError2 where
  pretty (WP2.MissingAssertion loc) = "Missing Assertion" <+> pretty loc
  pretty (WP2.MissingBound loc) = "Missing Bound" <+> pretty loc
  pretty (WP2.ExcessBound loc) = "Excess Bound" <+> pretty loc
  pretty (WP2.MissingPostcondition loc) = "Missing Postcondition" <+> pretty loc
  pretty (WP2.DigHole loc) = "Dig Hole" <+> pretty loc

instance Pretty TypeError where
  pretty (NotInScope name _) = "The definition" <+> pretty name <+> "is not in scope"
  pretty (UnifyFailed a b _) = "Cannot unify:"
    <+> pretty a <+> "with"
    <+> pretty b
  pretty (RecursiveType v a _) = "Recursive type variable: "
    <+> pretty v <+> "in" <+> pretty a
  pretty (NotFunction a _) = "The type" <+> pretty a <+> "is not a function type"

-- --------------------------------------------------------------------------------
-- -- | Val
--
-- instance Pretty Val where
--   pretty = pretty . show
