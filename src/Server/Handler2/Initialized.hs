{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Server.Handler2.Initialized (handler) where

import qualified Data.Text as Text

import qualified Language.LSP.Server as LSP
import qualified Language.LSP.Types  as LSP
import Server.Handler2.Utils
import Server.Monad (ServerM)

handler :: ServerM ()
handler = do
  logText "initialized"
  let requestParams =
        LSP.ShowMessageRequestParams
          LSP.MtInfo
          "GCL Server Initialized."
          Nothing
  _ <- LSP.sendRequest LSP.SWindowShowMessageRequest requestParams $ \case
      Right _ ->
        LSP.sendNotification LSP.SWindowShowMessage (LSP.ShowMessageParams LSP.MtInfo "BTW, nice to meet you!")
      Left err ->
        LSP.sendNotification LSP.SWindowShowMessage (LSP.ShowMessageParams LSP.MtError $ "Oops, something went wrong...\n" <> Text.pack (show err))
  return ()