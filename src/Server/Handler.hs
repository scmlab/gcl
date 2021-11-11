{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PolyKinds #-}

module Server.Handler
  ( handlers
  ) where

import           Control.Lens                   ( (^.) )
import qualified Data.Aeson                    as JSON
import           Language.LSP.Server            ( Handlers
                                                , notificationHandler
                                                , requestHandler
                                                )
import           Server.DSL
import           Server.Monad

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
    interpret uri (notificationResponder uri) $ do
      muted <- isMuted
      if muted
        then return []
        else do
          logText " ---> TextDocumentDidChange"
          source    <- getSource
          parsed    <- parse source
          persist (Parsed parsed)
          converted <- convert parsed
          persist (Converted converted)
          typeCheck (convertedProgram converted)
          result <- sweep converted
          persist (Swept result)
          generateResponseAndDiagnosticsFromCurrentState
  , notificationHandler J.STextDocumentDidOpen $ \ntf -> do
    let uri    = ntf ^. (J.params . J.textDocument . J.uri)
    let source = ntf ^. (J.params . J.textDocument . J.text)
    interpret uri (notificationResponder uri) $ do
      logText " ---> TextDocumentDidOpen"
      parsed    <- parse source
      persist (Parsed parsed)
      converted <- convert parsed
      persist (Converted converted)
      typeCheck (convertedProgram converted)
      result <- sweep converted
      persist (Swept result)
      generateResponseAndDiagnosticsFromCurrentState
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
      logText " <--- Syntax Highlighting"
      stage <- getState
      let legend = J.SemanticTokensLegend
            (J.List J.knownSemanticTokenTypes)
            (J.List J.knownSemanticTokenModifiers)
      let
        highlightings = case stage of
          Uninitialized _ -> []
          Parsed result -> parsedHighlighings result
          Converted result ->
            parsedHighlighings (convertedPreviousStage result)
          Swept result -> parsedHighlighings
            (convertedPreviousStage (sweptPreviousStage result))
      let tokens = J.makeSemanticTokens legend highlightings
      case tokens of
        Left t -> return $ Left $ J.ResponseError J.InternalError t Nothing
        Right tokens' -> return $ Right $ Just tokens'
  ]

ignoreErrors
  :: ([Error], Maybe (Either J.ResponseError (Maybe J.SemanticTokens)))
  -> Either J.ResponseError (Maybe J.SemanticTokens)
ignoreErrors (_, Nothing) = Left $ J.ResponseError J.InternalError "?" Nothing 
ignoreErrors (_, Just xs) = xs
