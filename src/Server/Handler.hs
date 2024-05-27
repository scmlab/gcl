{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE BlockArguments #-}

{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

module Server.Handler ( handlers ) where

import           Control.Lens                   ( (^.) )
import qualified Data.Aeson                     as JSON
import           Language.LSP.Server            ( Handlers
                                                , notificationHandler
                                                , requestHandler
                                                )

import qualified Language.LSP.Types             as LSP
import qualified Language.LSP.Types.Lens        as LSP

import qualified Server.Handler.Initialized    as Initialized
import qualified Server.Handler.GoToDefinition as GoToDefinition
import qualified Server.Handler.AutoCompletion as AutoCompletion
import qualified Server.Handler.SemanticTokens as SemanticTokens
import qualified Server.Handler.CustomMethod   as CustomMethod
import Server.Monad (ServerM, modifyPositionDelta)
import Server.PositionMapping (applyChange)
import Server.Load (load)

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
        Just filePath -> load filePath (\_ -> return ())
  , -- "textDocument/didChange" - after every edition
    notificationHandler LSP.STextDocumentDidChange $ \ntf -> do
      let uri        :: LSP.Uri = ntf ^. (LSP.params . LSP.textDocument . LSP.uri)
      let (LSP.List changes)    = ntf ^. (LSP.params . LSP.contentChanges)
      case LSP.uriToFilePath uri of
        Nothing       -> return ()
        Just filePath -> modifyPositionDelta filePath (\positionDelta -> foldl applyChange positionDelta changes)
  , -- "textDocument/completion" - autocompletion
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
  , -- "guabao" - reload, refine, inspect and etc.
    requestHandler (LSP.SCustomMethod "guabao") $ \req responder -> do
      let params = req ^. LSP.params
      CustomMethod.handler params (responder . Right . JSON.toJSON)
  ]




-- elaborate :: A.Program -> Either Error E.Program
-- elaborate abstract =  