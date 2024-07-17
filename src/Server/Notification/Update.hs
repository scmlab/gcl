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
import Error (Error)
import GCL.Predicate (Spec(..), PO(..), Origin(..))
import GCL.WP.Type (StructWarning(..))
import Server.SrcLoc (toLSPRange)
import qualified Language.LSP.Types as LSP
import qualified Data.Text as Text
import Data.Loc (Loc(..), Pos(..))

sendUpdateNotification :: FilePath -> [Error] -> ServerM ()
sendUpdateNotification filePath errors = do
  maybeFileState <- loadFileState filePath
  case maybeFileState of
    Nothing -> return ()
    Just fileState -> do
      let json :: JSON.Value = makeUpdateNotificationJson fileState errors
      Server.sendCustomNotification "guabao/update" json

makeUpdateNotificationJson :: FileState -> [Error] -> JSON.Value
makeUpdateNotificationJson FileState{specifications, proofObligations, warnings} errors = JSON.object
  [ "specs" .= JSON.toJSON specifications
  , "pos" .= JSON.toJSON proofObligations
  , "warnings" .= JSON.toJSON warnings
  , "errors" .= JSON.toJSON errors
  ]

instance JSON.ToJSON Spec where
  toJSON :: Spec -> JSON.Value
  toJSON Specification{..} = 
    object [ "id" .= JSON.toJSON (show specID)
           , "preCondition" .= JSON.toJSON (show specPreCond)
           , "postCondition" .= JSON.toJSON (show specPostCond)
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
    [ "assumption"    .= JSON.toJSON (show poPre)
    , "goal"          .= JSON.toJSON (show poPost)
    , "hash"          .= JSON.toJSON poAnchorHash
    , "proofLocation" .= case poAnchorLoc of
                          Nothing     -> JSON.Null
                          Just range  -> JSON.toJSON (toLSPRange range)
    , "origin"        .= JSON.toJSON poOrigin
    ]

locToJson :: Loc -> JSON.Value
locToJson NoLoc = object []
locToJson (Loc (Pos filePath line _ character) (Pos _ line' _ character')) = object
  [ "filePath" .= JSON.toJSON filePath
  , "start"    .= object
    ["line"       .= line
    , "character" .= character]
  , "end"      .= object
    ["line"       .= line'
    , "character" .= character']
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
    [ "location" .= locToJson originLoc
    , "explanation" .= JSON.String originHeader
    ]