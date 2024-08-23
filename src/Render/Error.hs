{-# LANGUAGE OverloadedStrings #-}

module Render.Error where

import           Data.Foldable                  ( toList )
import           Data.Loc                       ( locOf )
import           Data.Loc.Range
import           Error
import           GCL.Type                     ( TypeError(..) )
import           GCL.WP.Types                   ( StructError(..) )
import           Render.Class
import           Render.Element
import           Render.Syntax.Abstract         ( )
import           Render.Syntax.Common           ( )
import           Syntax.Parser.Error           ( ParseError(..) )

instance RenderSection Error where
  renderSection (ParseError     e   ) = renderSection e
  renderSection (TypeError      e   ) = renderSection e
  renderSection (StructError    e   ) = renderSection e
  renderSection (CannotReadFile path) = Section
    Red
    [ Header "Cannot Read File" Nothing
    , Paragraph $ "\"" <> render path <> "\" does not exist"
    ]
  renderSection (Others _ msg _) =
    Section Red [Header "Server Internal Error" Nothing, Paragraph $ render msg]

instance RenderSection ParseError where
  renderSection (LexicalError pos) =
    Section Red [Header "Lex Error" (Just $ Range pos pos)]
  renderSection (SyntacticError pairs _) = -- the logMsg (the second arg) was used for debugging
    Section Red
      $ mconcat
      $ map
          (\(loc, msg) ->
            [Header "Parse Error" (fromLoc loc), Paragraph $ render msg]
          )
      $ toList pairs

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
  renderSection (KindUnifyFailed s t loc) = Section
    Red
    [ Header "Cannot unify kinds" (fromLoc loc)
    , Paragraph $ "Cannot unify: " <> render s <> "\nwith        :" <+> render t
    ]
  renderSection (RecursiveType name t loc) = Section
    Red
    [ Header "Recursive type variable" (fromLoc loc)
    , Paragraph $ render name <+> "is recursive in" <+> render t
    ]
  renderSection (AssignToConst n) = Section
    Red
    [ Header "Assigned to const declaration" (fromLoc (locOf n))
    , Paragraph $ "Identifier" <+> render n <+> "cannot be assigned"
    ]
  renderSection (UndefinedType n) = Section
    Red
    [ Header "Undefined Type" (fromLoc (locOf n))
    , Paragraph $ "Type" <+> render n <+> "is undefined"
    ]
  renderSection (DuplicatedIdentifiers ids) = Section
    Red
    (concatMap
      (\n ->
        [ Header "Duplicated Identifiers" (fromLoc (locOf n))
        , Paragraph $ "Duplicated identifier" <+> render n
        ]
      )
      ids
    )
  renderSection (RedundantNames ns) = Section
    Red
    [ Header "Redundant Names" (fromLoc (locOf ns))
    , Paragraph $ "Redundant names" <+> renderManySepByComma ns
    ]
  renderSection (RedundantExprs exprs) = Section
    Red
    [ Header "Redundant Exprs" (fromLoc (locOf exprs))
    , Paragraph $ "Redundant exprs" <+> renderManySepByComma exprs
    ]
  renderSection (MissingArguments ns) = Section
    Red
    [ Header "Missing arguments" (fromLoc (locOf ns))
    , Paragraph $ "Missing arguments" <+> renderManySepByComma ns
    ]
  renderSection (PatternArityMismatch expected actual loc) = Section
    Red
    [ Header "Pattern arity mismatch" (fromLoc loc)
    , Paragraph $ "Expect" <+> render expected <+> "arguments but found" <+> render actual
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
  renderSection (LocalVarExceedScope loc) = Section
    Red
    [ Header "Local variable(s) exceeded scope" (fromLoc loc)
    , Paragraph "Variables defined in a block must not remain in preconditions out of the block"
    ]
