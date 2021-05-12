{-# LANGUAGE OverloadedStrings #-}

module Render.Error where

import Data.Loc.Range
import Error
import GCL.Type (TypeError (..))
import GCL.WP (StructError (..))
import Pretty hiding ((<+>))
import Render.Class
import Render.Element
import Render.Syntax.Common ()

instance RenderBlock Error where
  renderBlock (SyntacticError (loc, msg)) = blockE (Just "Parse Error") (fromLoc loc) (render msg)
  renderBlock (TypeError e) = renderBlock e
  renderBlock (StructError e) = renderBlock e
  renderBlock (CannotReadFile path) = blockE (Just "Cannot Read File") Nothing $ "\"" <> render path <> "\" does not exist"
  renderBlock (Others msg) = blockE (Just "Server Internal Error") Nothing $ render msg

instance RenderBlock TypeError where
  renderBlock (NotInScope name loc) =
    blockE (Just "Not In Scope") (fromLoc loc) $
      render name <+> "is not in scope"
  renderBlock (UnifyFailed s t loc) =
    blockE (Just "Cannot unify types") (fromLoc loc) $
      "Cannot unify: " <> render (pretty s)
        <> "\nwith        :" <+> render (pretty t)
  renderBlock (RecursiveType name t loc) =
    blockE (Just "Recursive type variable") (fromLoc loc) $
      render name
        <+> "is recursive in"
        <+> render (pretty t)
  renderBlock (NotFunction t loc) =
    blockE (Just "Not a function") (fromLoc loc) $
      "The type" <+> render (pretty t) <+> "is not a function type"

instance RenderBlock StructError where
  renderBlock (MissingAssertion loc) =
    blockE
      (Just "Missing Loop Invariant")
      (fromLoc loc)
      "There should be a loop invariant before the DO construct"
  renderBlock (MissingPostcondition loc) =
    blockE
      (Just "Missing Postcondition")
      (fromLoc loc)
      "The last statement of the program should be an assertion"