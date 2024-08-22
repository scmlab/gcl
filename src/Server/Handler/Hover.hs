
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE NamedFieldPuns #-}

module Server.Handler.Hover where

import qualified Language.LSP.Types            as LSP
import qualified Server.SrcLoc                 as SrcLoc
import qualified Server.IntervalMap            as IntervalMap
import Server.PositionMapping (PositionDelta, toCurrentRange, PositionMapping(..), PositionResult(..), fromDelta)

import Server.Monad (ServerM, FileState (..), loadFileState, logText)

handler :: LSP.Uri -> LSP.Position -> (Maybe LSP.Hover -> ServerM ()) -> ServerM ()
handler uri lspPosition responder = do
  logText "hover: start\n"
  case LSP.uriToFilePath uri of
    Nothing       -> do
      logText "hover: failed - uri not valid\n"
      responder Nothing
    Just filePath -> do
      maybeFileState <- loadFileState filePath
      case maybeFileState of
        Nothing                           -> do
          logText "hover: failed - not loaded yet\n"
          responder Nothing
        Just FileState{hoverInfos, positionDelta, toOffsetMap} -> do
          case fromDelta positionDelta lspPosition of
            PositionExact oldLspPosition -> do
              let oldPos = SrcLoc.fromLSPPosition toOffsetMap filePath oldLspPosition
              case IntervalMap.lookup oldPos hoverInfos of
                Nothing             -> do
                  logText "hover: not exist - no information for this position\n"
                  responder Nothing
                Just hover -> do
                  logText "hover: success\n"
                  responder $ Just $ toCurrentHover positionDelta hover
                  logText "hover: response sent\n"
            _                            -> do
              logText "hover: not exist - new position after last reload\n"
              responder Nothing
  logText "hover: end\n"

toCurrentHover :: PositionDelta -> LSP.Hover -> LSP.Hover
toCurrentHover positionDelta (LSP.Hover contents maybeRange)
  = LSP.Hover contents (maybeRange >>= toCurrentRange (PositionMapping positionDelta))