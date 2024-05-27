{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE BlockArguments #-}

module Server.Handler.CustomMethod where

import qualified Data.Aeson.Types as JSON

import Server.Monad (ServerM)

import Server.Handler.CustomMethod.Types (Response (..), Request (..), ReqKind (..), ResKind (..))
import Server.Handler.CustomMethod.Reload as Reload
-- import qualified Server.Monad (convertErrorsToResponsesAndDiagnostics)


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
        ReqReload                         -> Reload.handler filePath
        -- ReqInspect range                  -> Inspect.handler range respondResult reportError
        -- ReqRefine2 range text             -> Refine.slowHandler range text respondResult reportError
        -- ReqInsertProofTemplate range hash -> InsertProofTemplate.slowHandler filePath range hash respondResult reportError
        -- ReqSubstitute redexNumber         -> SubstituteRedex.handler filePath redexNumber respondResult reportError
        -- ReqHelloWorld range               -> HelloWorld.handler range respondResult reportError
        _                                 -> error "Not implemented yet."
