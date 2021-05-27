{-# LANGUAGE OverloadedStrings #-}

module Render.Error where

import Data.Loc.Range
import Error
import GCL.Type (TypeError (..))
import GCL.WP (StructError (..))
import Render.Class
import Render.Element
import Render.Syntax.Common ()
import Render.Syntax.Abstract ()
import Data.Foldable (toList)

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
      "Cannot unify: " <> render s
        <> "\nwith        :" <+> render t
  renderBlock (RecursiveType name t loc) =
    blockE (Just "Recursive type variable") (fromLoc loc) $
      render name
        <+> "is recursive in"
        <+> render t
  renderBlock (NotFunction t loc) =
    blockE (Just "Not a function") (fromLoc loc) $
      "The type" <+> render t <+> "is not a function type"
  renderBlock (NotEnoughExprsInAssigment vars loc) =
    blockE (Just "Not Enough Expressions") (fromLoc loc) $
      "Variables" <+> renderManySepByComma (toList vars) <+> "do not have corresponing expressions in the assigment"
  renderBlock (TooManyExprsInAssigment exprs loc) =
    blockE (Just "Too Many Expressions") (fromLoc loc) $
      "Expressions" <+> renderManySepByComma (toList exprs) <+> "do not have corresponing variables in the assigment"

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