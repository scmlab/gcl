{-# LANGUAGE OverloadedStrings #-}

module Render.Error where

import           Data.Foldable                  ( toList )
import           Data.Loc.Range
import           Data.Loc                       ( locOf )
import           Error
import qualified GCL.Scope                     as Scope
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
  renderSection (ScopeError     e   ) = renderSection e
  renderSection (TypeError      e   ) = renderSection e
  renderSection (StructError    e   ) = renderSection e
  renderSection (CannotReadFile path) = Section
    Red
    [ Header "Cannot Read File" Nothing
    , Paragraph $ "\"" <> render path <> "\" does not exist"
    ]
  renderSection (Others msg) =
    Section Red [Header "Server Internal Error" Nothing, Paragraph $ render msg]

instance RenderSection Scope.ScopeError where
  renderSection (Scope.NotInScope name) = Section
    Red
    [ Header "Not In Scope" (fromLoc (locOf name))
    , Paragraph $ render name <+> "is not in scope"
    ]
  renderSection (Scope.DuplicatedIdentifiers ns) = Section Red $ concatMap
    (\name ->
      [ Header "Duplicated Identifier" (fromLoc (locOf name))
      , Paragraph $ render name
      ]
    )
    ns
  renderSection (Scope.RedundantNames ns) = Section
    Red
    [ Header "Redundant Names" (fromLoc (locOf ns))
    , Paragraph $ renderManySepByComma ns
    ]
  renderSection (Scope.RedundantPatterns patts) = Section
    Red
    [ Header "Redundant Patterns" (fromLoc (locOf patts))
    , Paragraph $ renderManySepByComma patts
    ]
  renderSection (Scope.RedundantExprs exprs) = Section
    Red
    [ Header "Redundant Exprs" (fromLoc (locOf exprs))
    , Paragraph $ renderManySepByComma exprs
    ]

instance RenderSection TypeError where
  renderSection (NotInScope name) = Section
    Red
    [ Header "Not In Scope" (fromLoc (locOf name))
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
  renderSection (UndefinedType n) = Section
    Red
    [ Header "Undefined Type" (fromLoc (locOf n))
    , Paragraph $ "Type" <+> render n <+> "is undefined"
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
