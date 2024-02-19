{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use forM_" #-}

module Server.Handler2.GoToDefinition (handler) where

import qualified Language.LSP.Types as LSP
import Server.Monad (ServerM, LoadedProgram (..))


import qualified Server.SrcLoc as SrcLoc
import qualified Server.IntervalMap as IntervalMap

import Server.Handler2.Utils

handler :: LSP.Uri -> LSP.Position -> ([LSP.LocationLink] -> ServerM ()) -> ServerM ()
handler uri position responder = do
  case LSP.uriToFilePath uri of
    Nothing       -> return ()
    Just filePath -> do
      logText "<-- Goto Definition"
      maybeSource <- getSource filePath
      case maybeSource of
        Nothing -> responder []
        Just source -> do
          let table = SrcLoc.makeToOffset source
          let positions' = SrcLoc.fromLSPPosition table filePath position

          maybeLoadedProgram <- dumpProgram filePath
          case maybeLoadedProgram of
            Nothing -> responder []
            Just loadedProgram -> case IntervalMap.lookup positions' (_scopingInfo loadedProgram) of
              Nothing -> responder []
              Just locationLink -> responder [locationLink]