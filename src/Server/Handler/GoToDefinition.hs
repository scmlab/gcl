{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use forM_" #-}

module Server.Handler.GoToDefinition where

import qualified Language.LSP.Types as LSP
import Server.Monad (ServerM, FileState (..), readSource, loadFileState, logText)


import qualified Server.SrcLoc as SrcLoc
import qualified Server.IntervalMap as IntervalMap
import Server.PositionMapping( PositionResult( PositionExact ), fromDelta )

handler :: LSP.Uri -> LSP.Position -> ([LSP.LocationLink] -> ServerM ()) -> ServerM ()
handler uri lspPosition responder = do
  logText "<-- Goto Definition"
  case LSP.uriToFilePath uri of
    Nothing       -> responder []
    Just filePath -> do
      maybeFileState <- loadFileState filePath
      case maybeFileState of
        Nothing                           -> responder []
        Just (FileState{definitionLinks, positionDelta}) -> do
          maybeSource <- readSource filePath
          case maybeSource of
            Nothing -> responder []
            Just source -> do
              let table = SrcLoc.makeToOffset source
              case fromDelta positionDelta lspPosition of
                PositionExact oldLspPosition -> do  
                  let oldPos = SrcLoc.fromLSPPosition table filePath oldLspPosition
                  case IntervalMap.lookup oldPos definitionLinks of
                    Nothing -> responder []
                    Just locationLink -> responder [locationLink]
                _                            -> responder []