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
  renderBlock (SyntacticError (pos, msg)) = Block (Just "Parse Error") (Just $ Range pos pos) Red (render msg) 
  renderBlock (TypeError e) = renderBlock e
  renderBlock (StructError e) = renderBlock e
  renderBlock (CannotReadFile path) = Block (Just "Cannot Read File") Nothing Red $ "\"" <> render path <> "\" does not exist" 
  renderBlock (Others msg) = Block (Just "Server Internal Error") Nothing Red $ render msg 

instance RenderBlock TypeError where
  renderBlock (NotInScope name loc) =
    Block (Just "Not In Scope") (fromLoc loc) Red $
      render name <+> "is not in scope"
  renderBlock (UnifyFailed s t loc) =
    Block (Just "Cannot unify types") (fromLoc loc) Red $
      "Cannot unify: " <> render s
        <> "\nwith        :" <+> render t
  renderBlock (RecursiveType name t loc) =
    Block (Just "Recursive type variable") (fromLoc loc) Red $
      render name
        <+> "is recursive in"
        <+> render t
  renderBlock (NotFunction t loc) =
    Block (Just "Not a function") (fromLoc loc) Red $
      "The type" <+> render t <+> "is not a function type"
  renderBlock (NotEnoughExprsInAssigment vars loc) =
    Block (Just "Not Enough Expressions") (fromLoc loc) Red $
      "Variables" <+> renderManySepByComma (toList vars) <+> "do not have corresponing expressions in the assigment"
  renderBlock (TooManyExprsInAssigment exprs loc) =
    Block (Just "Too Many Expressions") (fromLoc loc) Red $
      "Expressions" <+> renderManySepByComma (toList exprs) <+> "do not have corresponing variables in the assigment"
  renderBlock (AssignToConst n loc) =
    Block (Just "Assginment to Constant Declaration") (fromLoc loc) Red $
      "Declaration" <+> render n <+> "is a constant, not a variable"
  renderBlock (AssignToLet n loc) = 
    Block (Just "Assginment to Let Declaration") (fromLoc loc) Red $
      "Declaration" <+> render n <+> "is a let binding, not a variable"

instance RenderBlock StructError where
  renderBlock (MissingAssertion loc) =
    Block
      (Just "Missing Loop Invariant")
      (fromLoc loc)
      Red
      "There should be a loop invariant before the DO construct"
  renderBlock (MissingPostcondition loc) =
    Block
      (Just "Missing Postcondition")
      (fromLoc loc)
      Red
      "The last statement of the program should be an assertion"