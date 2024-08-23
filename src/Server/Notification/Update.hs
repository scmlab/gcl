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
import GCL.Predicate (Spec(..), PO(..), Origin(..))
import GCL.WP.Types (StructWarning(..))
import Server.SrcLoc (toLSPRange)
import           Pretty.Predicate               ( )
import Data.Text.Prettyprint.Doc
import Pretty.Typed ()


sendUpdateNotification :: FilePath -> ServerM ()
sendUpdateNotification filePath = do
  maybeFileState <- loadFileState filePath
  case maybeFileState of
    Nothing -> return ()
    Just fileState -> do
      let json :: JSON.Value = makeUpdateNotificationJson filePath fileState
      Server.sendCustomNotification "gcl/update" json


makeUpdateNotificationJson :: FilePath -> FileState -> JSON.Value
makeUpdateNotificationJson filePath FileState{specifications, proofObligations, warnings} = JSON.object
  [ "filePath" .= JSON.toJSON filePath
  , "specs" .= JSON.toJSON (map snd specifications)
  , "pos" .= JSON.toJSON (map snd proofObligations)
  , "warnings" .= JSON.toJSON (map snd warnings)
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

--         tag: "Abort" | "Skip" | "Spec" | "Assignment" | "Conditional" | "Loop invariant" | "Loop termination";
instance JSON.ToJSON Origin where
  toJSON :: Origin -> JSON.Value
  toJSON (AtAbort loc) = object
    [ "tag"      .= JSON.String "Abort"
    , "location" .= JSON.toJSON loc
    ]
  toJSON (AtSkip loc) = object
    [ "tag"      .= JSON.String "Skip"
    , "location" .= JSON.toJSON loc
    ]
  toJSON (AtSpec loc) = object
    [ "tag"      .= JSON.String "Spec"
    , "location" .= JSON.toJSON loc
    ]
  toJSON (AtAssignment loc) = object
    [ "tag"      .= JSON.String "Assignment"
    , "location" .= JSON.toJSON loc
    ]
  toJSON (AtAssertion loc) = object
    [ "tag"      .= JSON.String "Assertion"
    , "location" .= JSON.toJSON loc
    ]
  toJSON (AtIf loc) = object
    [ "tag"      .= JSON.String "Conditional"
    , "location" .= JSON.toJSON loc
    ]
  toJSON (AtLoop loc) = object
    [ "tag"      .= JSON.String "Loop invariant"
    , "location" .= JSON.toJSON loc
    ]
  toJSON (AtTermination loc) = object
    [ "tag"      .= JSON.String "Loop termination"
    , "location" .= JSON.toJSON loc
    ]
  toJSON Explain{originHeader, originLoc} = object
    [ "tag" .= JSON.String originHeader
    , "location" .= JSON.toJSON originLoc
    ]
