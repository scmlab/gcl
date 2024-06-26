
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
handler uri lspPosition responder =do
  logText "<-- Goto Definition"
  case LSP.uriToFilePath uri of
    Nothing       -> responder Nothing
    Just filePath -> do
      maybeFileState <- loadFileState filePath
      case maybeFileState of
        Nothing                           -> responder Nothing
        Just FileState{hoverInfos, positionDelta, toOffsetMap} -> do
          case fromDelta positionDelta lspPosition of
            PositionExact oldLspPosition -> do
              let oldPos = SrcLoc.fromLSPPosition toOffsetMap filePath oldLspPosition
              case IntervalMap.lookup oldPos hoverInfos of
                Nothing             -> responder Nothing
                Just (hover, _type) -> responder $ Just $ toCurrentHover positionDelta hover
            _                            -> responder Nothing

toCurrentHover :: PositionDelta -> LSP.Hover -> LSP.Hover
toCurrentHover positionDelta (LSP.Hover contents maybeRange)
  = LSP.Hover contents (maybeRange >>= toCurrentRange (PositionMapping positionDelta))