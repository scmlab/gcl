{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PolyKinds #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

module Server.Handler2 ( handlers ) where

import           Control.Lens                   ( (^.) )
import qualified Data.Aeson                     as JSON
import           Language.LSP.Server            ( Handlers
                                                , notificationHandler
                                                , requestHandler
                                                )
import           Server.Monad                   hiding (logText)

import qualified Language.LSP.Types             as LSP
import qualified Language.LSP.Types.Lens        as LSP
import qualified Server.Handler2.Initialized    as Initialized
import qualified Server.Handler.AutoCompletion as AutoCompletion
import qualified Server.Handler2.CustomMethod   as CustomMethod
import qualified Server.Handler2.GoToDefinition as GoToDefinition
import qualified Server.Handler2.Hover          as Hover
import qualified Server.Handler2.SemanticTokens as SemanticTokens
import qualified Server.Handler2.Reload         as Reload

-- handlers of the LSP server
handlers :: Handlers ServerM
handlers = mconcat
  [ -- "initialized" - after initialize
    notificationHandler LSP.SInitialized $ \_ntf -> do
      Initialized.handler
  , -- "textDocument/completion" - autocompletion
    requestHandler LSP.STextDocumentCompletion $ \req responder -> do
      let completionContext = req ^. LSP.params . LSP.context
      let position          = req ^. LSP.params . LSP.position
      AutoCompletion.handler position completionContext >>= (responder . Right . InR)
  , -- "textDocument/definition" - go to definition
    requestHandler LSP.STextDocumentDefinition $ \req responder -> do
      let uri = req ^. (LSP.params . LSP.textDocument . LSP.uri)
      let pos = req ^. (LSP.params . LSP.position)
      GoToDefinition.handler uri pos (responder . Right . LSP.InR . LSP.InR . LSP.List)
  , -- "textDocument/hover" - get hover information
    requestHandler LSP.STextDocumentHover $ \req responder -> do
      let uri = req ^. (LSP.params . LSP.textDocument . LSP.uri)
      let pos = req ^. (LSP.params . LSP.position)
      Hover.handler uri pos (responder . Right)
  , -- "textDocument/semanticTokens/full" - get all semantic tokens
    requestHandler LSP.STextDocumentSemanticTokensFull $ \req responder -> do
      let uri = req ^. (LSP.params . LSP.textDocument . LSP.uri)
      SemanticTokens.handler uri responder
  , -- "guabao" - reload, refine, inspect and etc.
    requestHandler (LSP.SCustomMethod "guabao") $ \req responder -> do
      let params = req ^. LSP.params
      CustomMethod.handler params (responder . Right . JSON.toJSON)
  , -- "guabao/reload"
    requestHandler (LSP.SCustomMethod "guabao/reload") $ \req responder -> do
      let params = req ^. LSP.params
      Reload.handler params (responder . Right . JSON.toJSON)
  ]

