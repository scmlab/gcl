{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE BlockArguments #-}

{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

module Server.Handler ( handlers ) where

import           Control.Monad                  ( when )
import           Control.Lens                   ( (^.) )
import qualified Data.Aeson                     as JSON
import Data.Text (Text)
import           Language.LSP.Server            ( Handlers
                                                , notificationHandler
                                                , requestHandler
                                                )

import qualified Language.LSP.Types             as LSP
import qualified Language.LSP.Types.Lens        as LSP
import qualified Language.LSP.Server            as LSP

import qualified Server.Handler.Initialized    as Initialized
import qualified Server.Handler.GoToDefinition as GoToDefinition
import qualified Server.Handler.AutoCompletion as AutoCompletion
import qualified Server.Handler.SemanticTokens as SemanticTokens
import qualified Server.Handler.Guabao.Reload  as Reload
import Server.Monad (ServerM, FileState (..), modifyFileState)
import Server.PositionMapping (applyChange, mkDelta)
import Server.Load (load)
import GCL.Predicate (Spec(..))
import qualified Server.Handler.OnDidChangeTextDocument as OnDidChangeTextDocument

-- handlers of the LSP server
handlers :: Handlers ServerM
handlers = mconcat
  [ -- "initialized" - after initialize
    notificationHandler LSP.SInitialized $ \_ntf -> do
      Initialized.handler
  , -- "textDocument/didOpen" - after open
    notificationHandler LSP.STextDocumentDidOpen $ \ntf -> do
      let uri            = ntf ^. (LSP.params . LSP.textDocument . LSP.uri)
      case LSP.uriToFilePath uri of
        Nothing       -> return ()
        Just filePath -> load filePath (\_ -> return ()) (\_ -> return ())
  , -- "textDocument/didChange" - after every edition
    notificationHandler LSP.STextDocumentDidChange $ \ntf -> do
      let uri        :: LSP.Uri = ntf ^. (LSP.params . LSP.textDocument . LSP.uri)
      let (LSP.List changes)    = ntf ^. (LSP.params . LSP.contentChanges)
      case LSP.uriToFilePath uri of
        Nothing       -> return ()
        Just filePath -> OnDidChangeTextDocument.handler filePath changes
  , -- "textDocument/completion" - auto-completion
    requestHandler LSP.STextDocumentCompletion $ \req responder -> do
      let completionContext = req ^. LSP.params . LSP.context
      let position          = req ^. LSP.params . LSP.position
      AutoCompletion.handler position completionContext >>= (responder . Right . LSP.InR)
  , -- "textDocument/definition" - go to definition
    requestHandler LSP.STextDocumentDefinition $ \req responder -> do
      let uri      = req ^. (LSP.params . LSP.textDocument . LSP.uri)
      let position = req ^. (LSP.params . LSP.position)
      GoToDefinition.handler uri position (responder . Right . LSP.InR . LSP.InR . LSP.List)
  -- , -- "textDocument/hover" - get hover information
    -- requestHandler LSP.STextDocumentHover $ \req responder -> do
    --   let uri = req ^. (LSP.params . LSP.textDocument . LSP.uri)
    --   let pos = req ^. (LSP.params . LSP.position)
    --   Hover.handler uri pos (responder . Right)
  , -- "textDocument/semanticTokens/full" - get all semantic tokens
    requestHandler LSP.STextDocumentSemanticTokensFull $ \req responder -> do
      let uri = req ^. (LSP.params . LSP.textDocument . LSP.uri)
      SemanticTokens.handler uri responder
  , -- "guabao/reload" - reload
    requestHandler (LSP.SCustomMethod "guabao/reload") $ jsonMiddleware Reload.handler
  ]

type CustomMethodHandler params result error = params -> (result -> ServerM ()) -> (error -> ServerM ()) -> ServerM ()

jsonMiddleware :: (JSON.FromJSON params, JSON.ToJSON result, JSON.ToJSON error)
                  => CustomMethodHandler params result error
                  -> LSP.Handler ServerM (LSP.CustomMethod :: LSP.Method LSP.FromClient LSP.Request)
jsonMiddleware handler req responder = do
  let json = req ^. LSP.params
  case decodeMessageParams json of
    Left error   -> responder (Left error)
    Right params -> do
      handler params
        (responder . Right . JSON.toJSON)
        (responder. Left . makeInternalError)

decodeMessageParams :: forall a. JSON.FromJSON a => JSON.Value -> Either LSP.ResponseError a
decodeMessageParams json = do
  case JSON.fromJSON json :: JSON.Result [a] of
    JSON.Success (params:[]) -> Right params
    JSON.Success _            -> error "should not happen"
    JSON.Error msg            -> Left (makeParseError "Json decoding failed.")

makeInternalError :: JSON.ToJSON e => e -> LSP.ResponseError
makeInternalError error = LSP.ResponseError 
    { _code    = LSP.InternalError
    , _message = ""
    , _xdata   = Just (JSON.toJSON error)
    }

makeParseError :: Text -> LSP.ResponseError
makeParseError message = LSP.ResponseError 
    { _code    = LSP.ParseError
    , _message = message
    , _xdata   = Nothing
    }
