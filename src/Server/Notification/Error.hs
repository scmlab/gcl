{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE InstanceSigs #-}

module Server.Notification.Error where

import qualified Data.Aeson as JSON
import Data.Aeson ((.=), object)
import qualified Server.Monad as Server
import           Pretty.Predicate               ( )
import Data.Loc (Loc(..), Pos (..))
import Data.Text.Prettyprint.Doc
import qualified Data.Text as Text
import Pretty.Typed ()
import Data.List.NonEmpty (NonEmpty ((:|)))
import Syntax.Parser.Error (ParseError(..))
import GCL.Type (TypeError(..))
import Error (Error (..))
import Server.Monad (ServerM)
import GCL.WP.Types (StructError (..))


sendErrorNotification :: FilePath -> [Error] -> ServerM ()
sendErrorNotification filePath errors = do
  let json :: JSON.Value = makeErrorNotificationJson filePath errors
  Server.sendCustomNotification "gcl/error" json

makeErrorNotificationJson :: FilePath -> [Error] -> JSON.Value
makeErrorNotificationJson filePath errors = JSON.object
  [ "filePath" .= JSON.toJSON filePath
  , "errors" .= JSON.toJSON errors
  ]

instance JSON.ToJSON Error where
  toJSON :: Error -> JSON.Value
  toJSON (CannotReadFile filePath) = object
    [ "tag" .= JSON.String "CannotReadFile"
    , "filePath" .= JSON.toJSON filePath
    ]
  toJSON (ParseError err) = object
    [ "tag" .= JSON.String "ParseError"
    , "message" .= JSON.toJSON err
    ]
  toJSON (TypeError err) = object
    [ "tag" .= JSON.String "TypeError"
    , "message" .= JSON.toJSON err
    ]
  toJSON (StructError err) = object
    [ "tag" .= JSON.String "StructError"
    , "message" .= JSON.toJSON err
    ]
  toJSON (Others title message loc) = object
    [ "tag" .= JSON.String "Others"
    , "title" .= JSON.toJSON title
    , "message" .= JSON.toJSON message
    , "location" .= JSON.toJSON loc
    ]

toLspPositionJSON :: Pos -> JSON.Value
toLspPositionJSON (Pos filepath line column offset) = object
    [ "line" .= JSON.toJSON (line - 1)
    , "character" .= JSON.toJSON (column - 1)
    ]


instance JSON.ToJSON ParseError where
  toJSON :: ParseError -> JSON.Value
  toJSON (LexicalError position) = object
    [ "tag" .= JSON.String "LexicalError"
    , "position" .= toLspPositionJSON position
    ]
  toJSON (SyntacticError locatedSymbols message) = object
    [ "tag" .= JSON.String "SyntacticError"
    , "locatedSymbols" .= JSON.toJSON (locatedSymbolsToJSON locatedSymbols)
    , "message" .= JSON.toJSON message
    ]
    where
      locatedSymbolsToJSON :: NonEmpty (Loc, String) -> JSON.Value
      locatedSymbolsToJSON (x :| xs) = JSON.toJSON $ map (\(loc, s) -> object [
          "location" .= JSON.toJSON loc
        , "symbol" .= JSON.toJSON s
        ]) (x:xs)

instance JSON.ToJSON TypeError where
  toJSON :: TypeError -> JSON.Value
  toJSON (NotInScope symbol) = object
    [ "tag" .= JSON.String "NotInScope"
    , "symbol" .= JSON.toJSON symbol
    ]
  toJSON (UnifyFailed type1 type2 loc) = object
    [ "tag" .= JSON.String "UnifyFailed"
    , "location" .= JSON.toJSON loc
    , "typeExpressions" .= JSON.toJSON (map (show . pretty) [type1, type2])
    ]
  toJSON (RecursiveType n t loc) = object
    [ "tag" .= JSON.String "RecursiveType"
    , "typeVariable" .= JSON.toJSON n
    , "typeExpression" .= t
    , "location" .= JSON.toJSON loc
    ]
  toJSON (AssignToConst name) = object
    [ "tag" .= JSON.String "AssignToConst"
    , "constSymbol" .= JSON.toJSON name
    ]
  toJSON (UndefinedType name) = object
    [ "tag" .= JSON.String "UndefinedType"
    , "typeVariable" .= JSON.toJSON name
    ]
  toJSON (DuplicatedIdentifiers names) = object
    [ "tag" .= JSON.String "DuplicatedIdentifiers"
    , "identifiers" .= JSON.toJSON (map JSON.toJSON names)
    ]
  toJSON (RedundantNames names) = object
    [ "tag" .= JSON.String "RedundantNames"
    , "names" .= JSON.toJSON (map JSON.toJSON names)
    ]
  toJSON (RedundantExprs expressions) = object
    [ "tag" .= JSON.String "RedundantExprs"
    , "expressions" .= JSON.toJSON (map (JSON.String . Text.pack . show . pretty) expressions)
    ]
  toJSON (MissingArguments names) = object
    [ "tag" .= JSON.String "MissingArguments"
    , "argumentNames" .= JSON.toJSON (map JSON.toJSON names)
    ]
  -- FIXME: Implement these.
  toJSON (KindUnifyFailed kind1 kind2 loc) = object
    [ "tag" .= JSON.String "KindUnifyFailed"
    , "location" .= JSON.toJSON loc
    , "kindExpressions" .= JSON.toJSON (map (show . pretty) [kind1, kind2])
    ]
  toJSON (PatternArityMismatch expected actual loc) = object
    [ "tag" .= JSON.String "PatternArityMismatch"
    , "location" .= JSON.toJSON loc
    , "expected" .= JSON.toJSON expected
    , "received" .= JSON.toJSON actual
    ]

instance JSON.ToJSON StructError where
  toJSON :: StructError -> JSON.Value
  toJSON (MissingAssertion loc) = object
    [ "tag" .= JSON.String "MissingAssertion"
    , "location" .= JSON.toJSON loc
    ]
  toJSON (MissingPostcondition loc) = object
    [ "tag" .= JSON.String "MissingPostcondition"
    , "location" .= JSON.toJSON loc
    ]
  toJSON (MultiDimArrayAsgnNotImp loc) = object
    [ "tag" .= JSON.String "MultiDimArrayAsgnNotImp"
    , "location" .= JSON.toJSON loc
    ]
  toJSON (LocalVarExceedScope loc) = object
    [ "tag" .= JSON.String "LocalVarExceedScope"
    , "location" .= JSON.toJSON loc
    ]