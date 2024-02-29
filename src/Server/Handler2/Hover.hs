{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}

module Server.Handler2.Hover
  ( handler
  ) where

import           Data.Loc                       ( posCoff )
import           Language.LSP.Types      hiding ( Range )
import           Pretty                         ( toText )
import qualified Server.SrcLoc                 as SrcLoc
import qualified Server.IntervalMap               as IntervalMap

import qualified Language.LSP.Server as LSP
import qualified Language.LSP.Types  as LSP

import Server.Handler2.Utils
import Server.Monad (ServerM, LoadedProgram (..))
import Server.Handler2.CustomMethod.Reload (reload)
import qualified Data.Text as Text
import Data.Aeson (encode)
import Server.CustomMethod

handler :: Uri -> Position -> (Maybe Hover -> ServerM ()) -> ServerM ()
handler uri position responder = case uriToFilePath uri of
  Nothing       -> responder Nothing
  Just filepath -> do
    logText "initialized"
    let requestParams =
          LSP.ShowMessageRequestParams
            LSP.MtInfo
            (Text.pack . show . encode $ Req filepath ReqReload)
            Nothing
    _ <- LSP.sendRequest LSP.SWindowShowMessageRequest requestParams $ \case
        Right _ ->
          LSP.sendNotification LSP.SWindowShowMessage (LSP.ShowMessageParams LSP.MtInfo "BTW, nice to meet you!")
        Left err ->
          LSP.sendNotification LSP.SWindowShowMessage (LSP.ShowMessageParams LSP.MtError $ "Oops, something went wrong...\n" <> Text.pack (show err))
    -- reload filepath (const $ pure()) (const $ pure())
    maybeSource <- getSource filepath
    case maybeSource of
      Nothing -> responder Nothing
      Just source -> do
        let table = SrcLoc.makeToOffset source
        let pos   = SrcLoc.fromLSPPosition table filepath position
        logText $ " ---> Hover " <> toText (posCoff pos)
        
        maybeLoadedProgram <- dumpProgram filepath
        case maybeLoadedProgram of
          Nothing -> responder Nothing
          Just loadedProgram -> do
            case IntervalMap.lookup pos (_typeCheckingInfo loadedProgram) of
              Nothing -> do
                  -- logText $ toText xs
                  logText "    < Hover (not found)"
                  responder Nothing
              Just (hover, _) -> do
                  logText $ "    < Hover " <> toText hover
                  responder (Just hover)