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
import           Language.LSP.Server
import           Language.LSP.Types      hiding ( Range
                                                , TextDocumentSyncClientCapabilities(..)
                                                )
import           Server.DSL
import           Server.Interpreter.RealWorld

-- import qualified Language.LSP.Types            as J
import qualified Language.LSP.Types.Lens       as J
import qualified Server.Handler.AutoCompletion as AutoCompletion
import qualified Server.Handler.CustomMethod   as CustomMethod

-- handlers of the LSP server
handlers :: Handlers ServerM
handlers = mconcat
  [ -- autocompletion
    requestHandler STextDocumentCompletion $ \req responder -> do
    let completionContext = req ^. J.params . J.context
    let position          = req ^. J.params . J.position
    AutoCompletion.handler position completionContext >>= responder . Right
  ,
    -- custom methods, not part of LSP
    requestHandler (SCustomMethod "guabao") $ \req responder -> do
    let params = req ^. J.params
    CustomMethod.handler params (responder . Right . JSON.toJSON)
  , notificationHandler STextDocumentDidChange $ \ntf -> do
    m <- getMute
    logText $ " --> TextDocumentDidChange (muted: " <> Text.pack (show m) <> ")"
    unless m $ do
      let
        NotificationMessage _ _ (DidChangeTextDocumentParams (VersionedTextDocumentIdentifier uri _) change)
          = ntf
      logText $ Text.pack $ " --> " <> show change
      case uriToFilePath uri of
        Nothing       -> pure ()
        Just filepath -> do
          interpret filepath (notificationResponder filepath) $ do
            source  <- getSource
            program <- parseProgram source
            typeCheck program
            result <- sweep program
            cacheResult (Right result)
            generateResponseAndDiagnosticsFromResult (Right result)
  , notificationHandler STextDocumentDidOpen $ \ntf -> do
    logText " --> TextDocumentDidOpen"
    let
      NotificationMessage _ _ (DidOpenTextDocumentParams (TextDocumentItem uri _ _ source))
        = ntf
    case uriToFilePath uri of
      Nothing       -> pure ()
      Just filepath -> do
        interpret filepath (notificationResponder filepath) $ do
          program <- parseProgram source
          typeCheck program
          result <- sweep program
          cacheResult (Right result)
          generateResponseAndDiagnosticsFromResult (Right result)
  ]
