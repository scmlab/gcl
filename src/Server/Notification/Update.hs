{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE InstanceSigs #-}

module Server.Notification.Update where

import qualified Data.Aeson as JSON
import Data.Aeson ((.=), object)
import qualified Server.Monad as Server
import Server.Monad (FileState(..), ServerM, loadFileState)
import Error (Error (..))
import GCL.Predicate (Spec(..), PO(..), Origin(..))
import GCL.WP.Types (StructWarning(..), StructError(..))
import Server.SrcLoc (toLSPRange)
import           Pretty.Predicate               ( )
import Data.Loc (Loc(..), Pos(..))
import Syntax.Common.Types (Name(..))
import Data.Text.Prettyprint.Doc
import qualified Data.Text as Text
import Pretty.Typed ()
import Data.List.NonEmpty (NonEmpty ((:|)))
import Syntax.Parser.Error (ParseError(..))
import GCL.Type (TypeError(..))

sendUpdateNotification :: FilePath -> [Error] -> ServerM ()
sendUpdateNotification filePath errors = do
  maybeFileState <- loadFileState filePath
  case maybeFileState of
    Nothing -> return ()
    Just fileState -> do
      let json :: JSON.Value = makeUpdateNotificationJson filePath fileState errors
      Server.sendCustomNotification "guabao/update" json

makeUpdateNotificationJson :: FilePath -> FileState -> [Error] -> JSON.Value
makeUpdateNotificationJson filePath FileState{specifications, proofObligations, warnings} errors = JSON.object
  [ "filePath" .= JSON.toJSON filePath
  , "specs" .= JSON.toJSON (map snd specifications)
  , "pos" .= JSON.toJSON (map snd proofObligations)
  , "warnings" .= JSON.toJSON warnings
  , "errors" .= JSON.toJSON errors
  ]

instance JSON.ToJSON Spec where
  toJSON :: Spec -> JSON.Value
  toJSON Specification{..} =
    object [ "id" .= JSON.toJSON (show specID)
           , "preCondition" .= JSON.toJSON (show $ pretty specPreCond)
           , "postCondition" .= JSON.toJSON (show $ pretty specPostCond)
           , "specRange" .= JSON.toJSON (toLSPRange specRange)
           ]

instance JSON.ToJSON StructWarning where
  toJSON :: StructWarning -> JSON.Value
  toJSON (MissingBound range) =
    object [ "tag" .= JSON.String "MissingBound"
           , "range" .= JSON.toJSON (toLSPRange range)
           ]
  toJSON (ExcessBound range) =
    object [ "tag" .= JSON.String "ExcessBound"
           , "range" .= JSON.toJSON (toLSPRange range)
           ]

instance JSON.ToJSON PO where
  toJSON :: PO -> JSON.Value
  toJSON PO{..} = object
    [ "assumption"    .= JSON.toJSON (show $ pretty poPre)
    , "goal"          .= JSON.toJSON (show $ pretty poPost)
    , "hash"          .= JSON.toJSON poAnchorHash
    , "proofLocation" .= case poAnchorLoc of
                          Nothing     -> JSON.Null
                          Just range  -> JSON.toJSON (toLSPRange range)
    , "origin"        .= JSON.toJSON poOrigin
    ]

locToJson :: Loc -> JSON.Value
locToJson NoLoc = JSON.Null
locToJson (Loc (Pos filePath line column _) (Pos _ line' column' _)) = object
  [ "filePath" .= JSON.toJSON filePath
  , "start"    .= object
    ["line"       .= line
    , "character" .= column]
  , "end"      .= object
    ["line"       .= line'
    , "character" .= column']
  ]

nameToJson :: Name -> JSON.Value
nameToJson (Name text loc) = object
  [ "symbol" .= JSON.String text
  , "location" .= locToJson loc
  ]

--         tag: "Abort" | "Skip" | "Spec" | "Assignment" | "Conditional" | "Loop invariant" | "Loop termination";
instance JSON.ToJSON Origin where
  toJSON :: Origin -> JSON.Value
  toJSON (AtAbort loc) = object
    [ "tag"      .= JSON.String "Abort"
    , "location" .= locToJson loc
    ]
  toJSON (AtSkip loc) = object
    [ "tag"      .= JSON.String "Skip"
    , "location" .= locToJson loc
    ]
  toJSON (AtSpec loc) = object
    [ "tag"      .= JSON.String "Spec"
    , "location" .= locToJson loc
    ]
  toJSON (AtAssignment loc) = object
    [ "tag"      .= JSON.String "Assignment"
    , "location" .= locToJson loc
    ]
  toJSON (AtAssertion loc) = object
    [ "tag"      .= JSON.String "Assertion"
    , "location" .= locToJson loc
    ]
  toJSON (AtIf loc) = object
    [ "tag"      .= JSON.String "Conditional"
    , "location" .= locToJson loc
    ]
  toJSON (AtLoop loc) = object
    [ "tag"      .= JSON.String "Loop invariant"
    , "location" .= locToJson loc
    ]
  toJSON (AtTermination loc) = object
    [ "tag"      .= JSON.String "Loop termination"
    , "location" .= locToJson loc
    ]
  toJSON (Explain{originHeader, originLoc}) = object
    [ "tag" .= JSON.String originHeader
    , "location" .= locToJson originLoc
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
  toJSON (Others message) = object
    [ "tag" .= JSON.String "Others"
    , "message" .= JSON.toJSON message
    ]



instance JSON.ToJSON ParseError where
  toJSON :: ParseError -> JSON.Value
  toJSON (LexicalError position) = object
    [ "tag" .= JSON.String "LexicalError"
    , "position" .= JSON.toJSON position
    ]
  toJSON (SyntacticError locatedSymbols message) = object
    [ "tag" .= JSON.String "SyntacticError"
    , "locatedSymbols" .= JSON.toJSON (locatedSymbolsToJSON locatedSymbols)
    , "message" .= JSON.toJSON message
    ]
    where
      locatedSymbolsToJSON :: NonEmpty (Loc, String) -> JSON.Value
      locatedSymbolsToJSON (x :| xs) = JSON.toJSON $ map (\(loc, s) -> object [
          "location" .= locToJson loc
        , "symbol" .= JSON.toJSON s
        ]) (x:xs)

instance JSON.ToJSON TypeError where
  toJSON :: TypeError -> JSON.Value
  toJSON (NotInScope symbol) = object
    [ "tag" .= JSON.String "NotInScope"
    , "symbol" .= nameToJson symbol
    ]
  toJSON (UnifyFailed type1 type2 loc) = object
    [ "tag" .= JSON.String "UnifyFailed"
    , "location" .= locToJson loc
    , "typeExpressions" .= JSON.toJSON (map (show . pretty) [type1, type2])
    ]
  toJSON (RecursiveType n t loc) = object
    [ "tag" .= JSON.String "RecursiveType"
    , "typeVariable" .= nameToJson n
    , "typeExpression" .= t
    , "location" .= locToJson loc
    ]
  toJSON (AssignToConst name) = object
    [ "tag" .= JSON.String "AssignToConst"
    , "constSymbol" .= nameToJson name
    ]
  toJSON (UndefinedType name) = object
    [ "tag" .= JSON.String "UndefinedType"
    , "typeVariable" .= nameToJson name
    ]
  toJSON (DuplicatedIdentifiers names) = object
    [ "tag" .= JSON.String "DuplicatedIdentifiers"
    , "identifiers" .= JSON.toJSON (map nameToJson names)
    ]
  toJSON (RedundantNames names) = object
    [ "tag" .= JSON.String "RedundantNames"
    , "names" .= JSON.toJSON (map nameToJson names)
    ]
  toJSON (RedundantExprs expressions) = object
    [ "tag" .= JSON.String "RedundantExprs"
    , "expressions" .= JSON.toJSON (map (JSON.String . Text.pack . show . pretty) expressions)
    ]
  toJSON (MissingArguments names) = object
    [ "tag" .= JSON.String "MissingArguments"
    , "argumentNames" .= JSON.toJSON (map nameToJson names)
    ]

instance JSON.ToJSON StructError where
  toJSON :: StructError -> JSON.Value
  toJSON (MissingAssertion loc) = object
    [ "tag" .= JSON.String "MissingAssertion"
    , "location" .= locToJson loc
    ]
  toJSON (MissingPostcondition loc) = object
    [ "tag" .= JSON.String "MissingPostcondition"
    , "location" .= locToJson loc
    ]
  toJSON (MultiDimArrayAsgnNotImp loc) = object
    [ "tag" .= JSON.String "MultiDimArrayAsgnNotImp"
    , "location" .= locToJson loc
    ]
  toJSON (LocalVarExceedScope loc) = object
    [ "tag" .= JSON.String "LocalVarExceedScope"
    , "location" .= locToJson loc
    ]