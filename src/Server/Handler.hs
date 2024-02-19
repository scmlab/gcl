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
import           Server.Pipeline

import           Error                          ( Error )
import qualified Language.LSP.Types            as J
import qualified Language.LSP.Types.Lens       as J
import qualified Server.Handler.AutoCompletion as AutoCompletion
import qualified Server.Handler.CustomMethod   as CustomMethod
import qualified Server.Handler.CustomMethod2  as CustomMethod'
import qualified Server.Handler.GoToDefn       as GoToDefn
import qualified Server.Handler.Hover          as Hover

-- handlers of the LSP server
handlers :: Handlers ServerM
handlers = mconcat
  [ notificationHandler J.SInitialized $ \_not -> do
      pure ()
  , 
    -- autocompletion
    requestHandler J.STextDocumentCompletion $ \req responder -> do
    let completionContext = req ^. J.params . J.context
    let position          = req ^. J.params . J.position
    AutoCompletion.handler position completionContext >>= responder . Right
  ,
    -- custom methods, not part of LSP
    requestHandler (J.SCustomMethod "guabao") $ \req responder -> do
    let params = req ^. J.params
    CustomMethod.handler params (responder . Right . JSON.toJSON)
  , -- custom methods group 2, for restructuring
    requestHandler (J.SCustomMethod "guabao2") $ \req responder -> do
    let params = req ^. J.params
    CustomMethod'.handler params (responder . Right . JSON.toJSON)
  , notificationHandler J.STextDocumentDidChange $ \ntf -> do
    let uri = ntf ^. (J.params . J.textDocument . J.uri)
    interpret uri (customRequestToNotification uri) $ do
      muted <- isMuted
      if muted
        then return []
        else do
          logText "\n ---> TextDocumentDidChange"
          source      <- getSource
          parsed      <- parse source
          converted   <- convert parsed
          typeChecked <- typeCheck converted
          swept       <- sweep typeChecked
          generateResponseAndDiagnostics swept
  , notificationHandler J.STextDocumentDidOpen $ \ntf -> do
    let uri    = ntf ^. (J.params . J.textDocument . J.uri)
    let source = ntf ^. (J.params . J.textDocument . J.text)
    interpret uri (customRequestToNotification uri) $ do
      logText "\n ---> TextDocumentDidOpen"
      parsed      <- parse source
      converted   <- convert parsed
      typeChecked <- typeCheck converted
      swept       <- sweep typeChecked
      generateResponseAndDiagnostics swept
  , -- Goto Definition
    requestHandler J.STextDocumentDefinition $ \req responder -> do
    let uri = req ^. (J.params . J.textDocument . J.uri)
    let pos = req ^. (J.params . J.position)
    GoToDefn.handler uri pos (responder . Right . J.InR . J.InR . J.List)
  , -- Hover
    requestHandler J.STextDocumentHover $ \req responder -> do
    let uri = req ^. (J.params . J.textDocument . J.uri)
    let pos = req ^. (J.params . J.position)
    Hover.handler uri pos (responder . Right)
  , requestHandler J.STextDocumentSemanticTokensFull $ \req responder -> do
    let uri = req ^. (J.params . J.textDocument . J.uri)
    interpret uri (responder . ignoreErrors) $ do
      logText "\n ---> Syntax Highlighting"
      let legend = J.SemanticTokensLegend
            (J.List J.knownSemanticTokenTypes)
            (J.List J.knownSemanticTokenModifiers)
      stage <- load
      let
        highlightings = case stage of
          Raw    _      -> []
          Parsed result -> parsedHighlighings result
          Converted result ->
            parsedHighlighings (convertedPreviousStage result)
          TypeChecked result -> parsedHighlighings
            (convertedPreviousStage (typeCheckedPreviousStage result))
          Swept result -> parsedHighlighings
            (convertedPreviousStage
              (typeCheckedPreviousStage (sweptPreviousStage result))
            )
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
