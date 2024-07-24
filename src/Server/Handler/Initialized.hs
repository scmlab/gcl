{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Server.Handler.Initialized (handler) where

import qualified Data.Text as Text

import qualified Language.LSP.Server as LSP
import qualified Language.LSP.Types  as LSP
import Server.Monad (ServerM)

handler :: ServerM ()
handler = do
  -- logText "initialized"
  let requestParams =
        LSP.ShowMessageRequestParams
          LSP.MtInfo
          "GCL Server Initialized."
          Nothing
  _ <- LSP.sendRequest LSP.SWindowShowMessageRequest requestParams $ \_ -> return ()
  return ()