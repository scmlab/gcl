{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PolyKinds #-}

module Server.Handler
  ( handlers
  ) where

-- import qualified Server.CustomMethod as Custom

import           Control.Lens                   ( (^.) )
import           Control.Monad.Except
import qualified Data.Aeson                    as JSON
import qualified Data.Text                     as Text
import           Language.LSP.Server            ( Handlers
                                                , notificationHandler
                                                , requestHandler
                                                )
import           Server.DSL
import           Server.Interpreter.RealWorld

import qualified Language.LSP.Types            as J
import qualified Language.LSP.Types.Lens       as J
import qualified Server.Handler.AutoCompletion as AutoCompletion
import qualified Server.Handler.CustomMethod   as CustomMethod
import qualified Server.Handler.Definition     as Definition
import qualified Server.Handler.Hover          as Hover

-- handlers of the LSP server
handlers :: Handlers ServerM
handlers = mconcat
  [ -- autocompletion
    requestHandler J.STextDocumentCompletion $ \req responder -> do
    let completionContext = req ^. J.params . J.context
    let position          = req ^. J.params . J.position
    AutoCompletion.handler position completionContext >>= responder . Right
  ,
    -- custom methods, not part of LSP
    requestHandler (J.SCustomMethod "guabao") $ \req responder -> do
    let params = req ^. J.params
    CustomMethod.handler params (responder . Right . JSON.toJSON)
  , notificationHandler J.STextDocumentDidChange $ \ntf -> do
    m <- getMute
    logText $ " --> TextDocumentDidChange (muted: " <> Text.pack (show m) <> ")"
    unless m $ do
      let uri    = ntf ^. (J.params . J.textDocument . J.uri)
      let change = ntf ^. (J.params . J.contentChanges)
      logText $ Text.pack $ " --> " <> show change
      case J.uriToFilePath uri of
        Nothing       -> pure ()
        Just filepath -> do
          interpret filepath (notificationResponder filepath) $ do
            source  <- getSource
            program <- parseProgram source
            typeCheck program
            result <- sweep program
            cacheResult (Right result)
            generateResponseAndDiagnosticsFromResult (Right result)
  , notificationHandler J.STextDocumentDidOpen $ \ntf -> do
    logText " --> TextDocumentDidOpen"
    let uri    = ntf ^. (J.params . J.textDocument . J.uri)
    let source = ntf ^. (J.params . J.textDocument . J.text)
    case J.uriToFilePath uri of
      Nothing       -> pure ()
      Just filepath -> do
        interpret filepath (notificationResponder filepath) $ do
          program <- parseProgram source
          typeCheck program
          result <- sweep program
          cacheResult (Right result)
          generateResponseAndDiagnosticsFromResult (Right result)
  , -- Goto Definition
    requestHandler J.STextDocumentDefinition $ \req responder -> do
    logText "<-- Goto Definition"
    let uri = req ^. (J.params . J.textDocument . J.uri)
    let pos = req ^. (J.params . J.position)
    Definition.handler uri pos (responder . Right . J.InR . J.InR . J.List)
  , -- Hover
    requestHandler J.STextDocumentHover $ \req responder -> do
    logText "<-- Hover"
    let uri = req ^. (J.params . J.textDocument . J.uri)
    let pos = req ^. (J.params . J.position)
    Hover.handler uri pos (responder . Right)
  ]
