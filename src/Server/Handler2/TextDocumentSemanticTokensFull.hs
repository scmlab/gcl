{-# LANGUAGE OverloadedStrings #-}
module Server.Handler2.TextDocumentSemanticTokensFull where

import Server.Monad (ServerM, LoadedProgram (..))
import qualified Language.LSP.Types as LSP
import Server.Handler2.Utils

handler :: LSP.Uri -> (Either LSP.ResponseError (Maybe LSP.SemanticTokens) -> ServerM ()) -> ServerM ()
handler fileUri responder = do
  case LSP.uriToFilePath fileUri of
    Nothing       -> respondError (LSP.ResponseError LSP.InvalidParams "Invalid uri" Nothing)
    Just filePath -> do
      logText "\n ---> Syntax Highlighting"
      let legend = LSP.SemanticTokensLegend
            (LSP.List LSP.knownSemanticTokenTypes)
            (LSP.List LSP.knownSemanticTokenModifiers)
      maybeLoadedProgram <- dumpProgram filePath
      case maybeLoadedProgram of
        Nothing -> respondError (LSP.ResponseError LSP.ServerNotInitialized "Please reload before requesting for semantic tokens." Nothing)
        Just loadedProgram -> do
          case LSP.makeSemanticTokens legend $ _highlightingInfos loadedProgram of
            Left errorMessage    -> do
              respondError (LSP.ResponseError LSP.InternalError errorMessage Nothing)
            Right semanticTokens -> do
              respondResult semanticTokens
  where
    respondResult :: Maybe LSP.SemanticTokens -> ServerM ()
    respondResult result = responder (Right result)
    respondError :: Error -> ServerM ()
    respondError err = responder (Left err)