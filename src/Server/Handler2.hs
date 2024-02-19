{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PolyKinds #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use lambda-case" #-}

module Server.Handler
  ( handlers
  ) where

import           Control.Lens                   ( (^.) )
import qualified Data.Aeson                    as JSON
import           Language.LSP.Server            ( Handlers
                                                , notificationHandler
                                                , requestHandler
                                                )
import           Server.Monad                  hiding (logText)

import qualified Language.LSP.Types            as LSP
import qualified Language.LSP.Types.Lens       as LSP
import qualified Server.Handler.AutoCompletion as AutoCompletion
import qualified Server.Handler2.CustomMethod   as CustomMethod
import qualified Server.Handler2.GoToDefinition as GoToDefinition
import qualified Server.Handler2.Hover           as Hover
import qualified Server.Handler2.TextDocumentSemanticTokensFull as TextDocumentSemanticTokensFull

-- handlers of the LSP server
handlers :: Handlers ServerM
handlers = mconcat
  [ notificationHandler LSP.SInitialized $ \_not -> do
      return ()
  , -- autocompletion
    requestHandler LSP.STextDocumentCompletion $ \req responder -> do
    let completionContext = req ^. LSP.params . LSP.context
    let position          = req ^. LSP.params . LSP.position
    AutoCompletion.handler position completionContext >>= responder . Right
  , -- custom methods
    requestHandler (LSP.SCustomMethod "guabao2") $ \req responder -> do
    let params = req ^. LSP.params
    CustomMethod.handler params (responder . Right . JSON.toJSON)
  , -- Goto Definition
    requestHandler LSP.STextDocumentDefinition $ \req responder -> do
    let uri = req ^. (LSP.params . LSP.textDocument . LSP.uri)
    let pos = req ^. (LSP.params . LSP.position)
    GoToDefinition.handler uri pos (responder . Right . LSP.InR . LSP.InR . LSP.List)
  , -- Hover
    requestHandler LSP.STextDocumentHover $ \req responder -> do
    let uri = req ^. (LSP.params . LSP.textDocument . LSP.uri)
    let pos = req ^. (LSP.params . LSP.position)
    Hover.handler uri pos (responder . Right)
  , requestHandler LSP.STextDocumentSemanticTokensFull $ \req responder -> do
    let uri = req ^. (LSP.params . LSP.textDocument . LSP.uri)
    TextDocumentSemanticTokensFull.handler uri responder
  ]

