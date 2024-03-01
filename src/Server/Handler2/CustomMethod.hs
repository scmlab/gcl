{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE BlockArguments #-}

module Server.Handler2.CustomMethod where

import qualified Data.Aeson.Types as JSON

import Server.CustomMethod (Response (..), Request (..), ReqKind (..), ResKind (..))
import Server.Monad (ServerM)
import qualified Server.Monad (convertErrorsToResponsesAndDiagnostics)
import Error (Error(..))

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


handler :: JSON.Value -> (Response -> ServerM ()) -> ServerM ()
handler params responder = do
  case JSON.fromJSON params :: JSON.Result [Request] of
    JSON.Success (request:[]) -> dispatchRequest request
    JSON.Success _            -> error "should not happen"
    JSON.Error msg            -> responder $ CannotDecodeRequest $ show msg ++ "\n" ++ show params
  where
    dispatchRequest :: Request -> ServerM ()
    dispatchRequest _request@(Req filePath reqKind) = do
      case reqKind of
        ReqReload                         -> Reload.handler filePath respondResult reportError
        ReqInspect range                  -> Inspect.handler range respondResult reportError
        ReqRefine2 range text             -> Refine.slowHandler range text respondResult reportError
        ReqInsertProofTemplate range hash -> InsertProofTemplate.slowHandler filePath range hash respondResult reportError
        ReqSubstitute redexNumber         -> SubstituteRedex.handler filePath redexNumber respondResult reportError
        ReqHelloWorld range               -> HelloWorld.handler range respondResult reportError
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
