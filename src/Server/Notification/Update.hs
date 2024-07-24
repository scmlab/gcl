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
import           Pretty.Predicate               ( )
import Data.Loc (Loc(..), Pos(..))
import Data.Text.Prettyprint.Doc

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