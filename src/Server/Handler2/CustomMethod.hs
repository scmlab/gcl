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


handler :: JSON.Value -> (Response -> ServerM ()) -> ServerM ()
handler params responder = do
    -- JSON Value => Request => Response
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
    dispatchRequest :: Request -> ServerM ()
    dispatchRequest _request@(Req filePath reqKind) = do
      case reqKind of
        ReqReload                         -> Reload.handler filePath respondResult respondError
        ReqInspect range                  -> Inspect.handler range respondResult respondError
        ReqRefine' range text             -> Refine.slowHandler range text respondResult respondError
        ReqInsertProofTemplate range hash -> InsertProofTemplate.slowHandler filePath range hash respondResult respondError
        ReqSubstitute redexNumber         -> SubstituteRedex.handler redexNumber respondResult respondError
        ReqHelloWorld range               -> HelloWorld.handler range respondResult respondError
        _                                 -> respondError (Others "Not implemented yet.")
      where
        respondError :: Error -> ServerM ()
        respondError err = do
          (responsesFromError, diagnosticsFromError)
            <- Server.Monad.convertErrorsToResponsesAndDiagnostics [err]
          sendDiagnostics filePath diagnosticsFromError
          responder (Res filePath responsesFromError)
        respondResult :: [ResKind] -> ServerM ()
        respondResult results = responder (Res filePath results)
