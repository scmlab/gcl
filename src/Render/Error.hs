{-# LANGUAGE OverloadedStrings #-}

module Render.Error where

import           Data.Foldable                  ( toList )
import           Data.Loc.Range
import           Error
import           GCL.Type                       ( TypeError(..) )
import           GCL.WP.Type                    ( StructError(..) )
import           Render.Class
import           Render.Element
import           Render.Syntax.Abstract         ( )
import           Render.Syntax.Common           ( )

instance RenderSection Error where
  renderSection (SyntacticError (pos, msg)) = Section
    Red
    [Header "Parse Error" (Just $ Range pos pos), Paragraph $ render msg]
  renderSection (TypeError      e   ) = renderSection e
  renderSection (StructError    e   ) = renderSection e
  renderSection (CannotReadFile path) = Section
    Red
    [ Header "Cannot Read File" Nothing
    , Paragraph $ "\"" <> render path <> "\" does not exist"
    ]
  renderSection (Others msg) =
    Section Red [Header "Server Internal Error" Nothing, Paragraph $ render msg]

instance RenderSection TypeError where
  renderSection (NotInScope name loc) = Section
    Red
    [ Header "Not In Scope" (fromLoc loc)
    , Paragraph $ render name <+> "is not in scope"
    ]
  renderSection (UnifyFailed s t loc) = Section
    Red
    [ Header "Cannot unify types" (fromLoc loc)
    , Paragraph $ "Cannot unify: " <> render s <> "\nwith        :" <+> render t
    ]
  renderSection (RecursiveType name t loc) = Section
    Red
    [ Header "Recursive type variable" (fromLoc loc)
    , Paragraph $ render name <+> "is recursive in" <+> render t
    ]
  renderSection (NotFunction t loc) = Section
    Red
    [ Header "Not a function" (fromLoc loc)
    , Paragraph $ "The type" <+> render t <+> "is not a function type"
    ]
  renderSection (NotArray t loc) = Section
    Red
    [ Header "Not an array" (fromLoc loc)
    , Paragraph $ "The type" <+> render t <+> "is not an array"
    ]
  renderSection (NotEnoughExprsInAssigment vars loc) = Section
    Red
    [ Header "Not Enough Expressions" (fromLoc loc)
    , Paragraph
    $   "Variables"
    <+> renderManySepByComma (toList vars)
    <+> "do not have corresponing expressions in the assigment"
    ]
  renderSection (TooManyExprsInAssigment exprs loc) = Section
    Red
    [ Header "Too Many Expressions" (fromLoc loc)
    , Paragraph
    $   "Expressions"
    <+> renderManySepByComma (toList exprs)
    <+> "do not have corresponing variables in the assigment"
    ]
  renderSection (AssignToConst n loc) = Section
    Red
    [ Header "Assginment to Constant Declaration" (fromLoc loc)
    , Paragraph $ "Declaration" <+> render n <+> "is a constant, not a variable"
    ]
  renderSection (AssignToLet n loc) = Section
    Red
    [ Header "Assginment to Let Declaration" (fromLoc loc)
    , Paragraph
    $   "Declaration"
    <+> render n
    <+> "is a let binding, not a variable"
    ]
  renderSection (UndefinedType n loc) = Section
    Red
    [ Header "Undefined Type" (fromLoc loc)
    , Paragraph
    $ "Type"
    <+> render n
    <+> "is undefined"
    ]
  renderSection (DuplicatedIdentifier n loc) = Section
    Red
    [ Header "Duplicated Identifier" (fromLoc loc)
    , Paragraph
    $ "Identifier"
    <+> render n
    <+> "is duplicated"
    ]

instance RenderSection StructError where
  renderSection (MissingAssertion loc) = Section
    Red
    [ Header "Missing Loop Invariant" (fromLoc loc)
    , Paragraph "There should be a loop invariant before the DO construct"
    ]
  renderSection (MissingPostcondition loc) = Section
    Red
    [ Header "Missing Postcondition" (fromLoc loc)
    , Paragraph "The last statement of the program should be an assertion"
    ]
  renderSection (MultiDimArrayAsgnNotImp loc) = Section
    Red
    [ Header "Assignment to Multi-Dimensional Array" (fromLoc loc)
    , Paragraph "Not implemented yet"
    ]
