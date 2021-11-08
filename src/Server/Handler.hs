{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PolyKinds #-}

module Server.Handler
  ( handlers
  ) where

import           Control.Lens                   ( (^.) )
import qualified Data.Aeson                    as JSON
import qualified Data.Text                     as Text
import           Language.LSP.Server            ( Handlers
                                                , notificationHandler
                                                , requestHandler
                                                )
import           Server.DSL
import           Server.Monad

import           Data.Foldable                  ( toList )
import           Error                          ( Error )
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
    let uri    = ntf ^. (J.params . J.textDocument . J.uri)
    let change = ntf ^. (J.params . J.contentChanges)
    interpret uri (notificationResponder uri) $ do
      muted <- isMuted
      logText
        $  " --> TextDocumentDidChange (muted: "
        <> Text.pack (show muted)
        <> ")"
      logText $ Text.pack $ " --> " <> show change
      if muted
        then return []
        else do
          source               <- getSource
          (concrete, abstract) <- parseProgram source
          typeCheck abstract
          result <- sweep concrete abstract
          cacheResult (Right result)
          generateResponseAndDiagnosticsFromResult (Right result)
  , notificationHandler J.STextDocumentDidOpen $ \ntf -> do
    let uri    = ntf ^. (J.params . J.textDocument . J.uri)
    let source = ntf ^. (J.params . J.textDocument . J.text)
    interpret uri (notificationResponder uri) $ do
      logText " --> TextDocumentDidOpen"
      (concrete, abstract) <- parseProgram source
      typeCheck abstract
      result <- sweep concrete abstract
      cacheResult (Right result)
      generateResponseAndDiagnosticsFromResult (Right result)
  , -- Goto Definition
    requestHandler J.STextDocumentDefinition $ \req responder -> do
    let uri = req ^. (J.params . J.textDocument . J.uri)
    let pos = req ^. (J.params . J.position)
    Definition.handler uri pos (responder . Right . J.InR . J.InR . J.List)
  , -- Hover
    requestHandler J.STextDocumentHover $ \req responder -> do
    let uri = req ^. (J.params . J.textDocument . J.uri)
    let pos = req ^. (J.params . J.position)
    Hover.handler uri pos (responder . Right)
  , requestHandler J.STextDocumentSemanticTokensFull $ \req responder -> do
    let uri = req ^. (J.params . J.textDocument . J.uri)
    interpret uri (responder . ignoreErrors) $ do
      logText "<-- Syntax Highlighting"
      result <- readCachedResult
      let highlightings = toList $ case result of
            Nothing            -> mempty
            Just (Left  _    ) -> mempty
            Just (Right cache) -> cacheHighlighings cache
      let legend = J.SemanticTokensLegend
            (J.List J.knownSemanticTokenTypes)
            (J.List J.knownSemanticTokenModifiers)
      let tokens = J.makeSemanticTokens legend highlightings
      case tokens of
        Left t -> return $ Left $ J.ResponseError J.InternalError t Nothing
        Right tokens' -> return $ Right $ Just tokens'
  ]

ignoreErrors
  :: Either [Error] (Either J.ResponseError (Maybe J.SemanticTokens))
  -> Either J.ResponseError (Maybe J.SemanticTokens)
ignoreErrors (Left  _errors) = Right Nothing
ignoreErrors (Right xs     ) = xs
