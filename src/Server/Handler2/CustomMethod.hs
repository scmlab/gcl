{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE LambdaCase #-}

module Server.Handler2.CustomMethod where

import qualified Data.Aeson.Types as JSON

import Server.CustomMethod (Response (..), Request (..), ReqKind (..), ResKind (..))
import Server.Monad (ServerM)
import qualified Server.Monad (convertErrorsToResponsesAndDiagnostics)
import Error (Error(..))

import qualified Language.LSP.Server as LSP
import qualified Language.LSP.Types  as LSP
import Server.Handler2.Utils
import qualified Server.Handler2.CustomMethod.Reload     as Reload (handler)
import qualified Server.Handler2.CustomMethod.Inspect    as Inspect (handler)
import qualified Server.Handler2.CustomMethod.Refine     as Refine (slowHandler)
import qualified Server.Handler2.CustomMethod.InsertProofTemplate 
                                                         as InsertProofTemplate (slowHandler)
import qualified Server.Handler2.CustomMethod.SubstituteRedex
                                                        as SubstituteRedex (handler)
import qualified Server.Handler2.CustomMethod.HelloWorld as HelloWorld (handler)
import qualified Data.Text as Text
import Data.Aeson

handler :: JSON.Value -> (Response -> ServerM ()) -> ServerM ()
handler params responder = do
  case JSON.fromJSON params of
    JSON.Error msg -> do
      logText
        $  " --> CustomMethod: CannotDecodeRequest "
        <> Text.pack (show msg)
        <> " "
        <> Text.pack (show params)
      responder $ CannotDecodeRequest $ show msg ++ "\n" ++ show params
    JSON.Success request -> dispatchRequest request
  where
    dispatchRequest :: [Request] -> ServerM ()
    dispatchRequest request =
      case head request of
        Req filePath reqKind ->
          case reqKind of
            ReqReload                         -> Reload.handler filePath respondResult reportError
            ReqInspect range                  -> Inspect.handler range respondResult reportError
            ReqRefine2 range text             -> Refine.slowHandler range text respondResult reportError
            ReqInsertProofTemplate range hash -> InsertProofTemplate.slowHandler filePath range hash respondResult reportError
            ReqSubstitute redexNumber         -> SubstituteRedex.handler filePath redexNumber respondResult reportError
            ReqHelloWorld range               -> do
              let requestParams =
                    LSP.ShowMessageRequestParams
                      LSP.MtInfo
                      (Text.pack . show . encode $ ReqHelloWorld range)
                      Nothing
              _ <- LSP.sendRequest LSP.SWindowShowMessageRequest requestParams $ \case
                  Right _ ->
                    LSP.sendNotification LSP.SWindowShowMessage (LSP.ShowMessageParams LSP.MtInfo "Just saying hello again!")
                  Left err ->
                    LSP.sendNotification LSP.SWindowShowMessage (LSP.ShowMessageParams LSP.MtError $ "Something went wrong!\n" <> Text.pack (show err))
              HelloWorld.handler range respondResult reportError
            _                                 -> reportError (Others "Not implemented yet.")
          where
            reportError :: Error -> ServerM ()
            reportError err = do
              (responsesFromError, diagnosticsFromError)
                <- Server.Monad.convertErrorsToResponsesAndDiagnostics [err]
              sendDiagnostics filePath diagnosticsFromError
              responder (Res filePath responsesFromError)
            respondResult :: [ResKind] -> ServerM ()
            respondResult results = responder (Res filePath results)
