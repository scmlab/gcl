{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE TypeSynonymInstances, FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Pretty
  ( module Pretty.Abstract
  , module Pretty.GCL.WP
  ) where

import Data.Text.Prettyprint.Doc
import Control.Monad ((>=>))
import Prelude hiding (Ordering(..))
import Data.Loc
import Text.Megaparsec.Error (errorBundlePretty)

import Error
import Syntax.Parser.Lexer (LexicalError)
import Syntax.Parser (SyntacticError(..))
import Syntax.Abstract hiding (var)
-- import Syntax.Abstract (Expr(..), Lit(..), Op(..), classify, Type(..), TBase(..))
import Syntax.Concrete (Fixity(..))
import GCL.WP (Obligation(..), Specification(..))
import GCL.Type (TypeError(..))
-- import GCL.Exec.ExecMonad (Val(..))

import Pretty.Abstract
import Pretty.GCL.WP

--------------------------------------------------------------------------------
-- | Error

instance Pretty Error where
  pretty (LexicalError err) = "Lexical Error" <+> pretty err
  pretty (SyntacticError (loc, err)) = "Syntactic Error" <+> pretty loc <> line
    <> pretty err
  pretty (TypeError err) = "Type Error" <+> pretty (locOf err) <> line <> pretty err
  -- pretty (ConvertError err) = "AST Convert Error" <+> pretty (locOf err) <> line <> pretty err

instance Pretty LexicalError where
-- instance Pretty ConvertError where

instance Pretty TypeError where
  pretty (NotInScope name _) = "The definition" <+> pretty name <+> "is not in scope"
  pretty (UnifyFailed a b _) = "Cannot unify:"
    <+> pretty a <+> "with"
    <+> pretty b
  pretty (RecursiveType v a _) = "Recursive type variable: "
    <+> pretty v <+> "in" <+> pretty a
  pretty (NotFunction a _) = "The type" <+> pretty a <+> "is not a function type"

--------------------------------------------------------------------------------
-- | Misc

instance Pretty Loc where
  pretty = pretty . displayLoc

-- --------------------------------------------------------------------------------
-- -- | Val
--
-- instance Pretty Val where
--   pretty = pretty . show
