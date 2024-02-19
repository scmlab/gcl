{-# LANGUAGE OverloadedStrings #-}
module Server.Handler2.TextDocumentSemanticTokensFull where

import Server.Monad (ServerM, LoadedProgram (highlightingInfos))
import qualified Language.LSP.Types as LSP
import Server.Handler2.Utils

handler :: LSP.Uri -> (Either LSP.ResponseError (Maybe LSP.SemanticTokens) -> ServerM ()) -> ServerM ()
handler fileUri responder = do

  case LSP.uriToFilePath fileUri of
    Nothing       -> return ()
    Just filePath -> do
      logText "\n ---> Syntax Highlighting"
      let legend = LSP.SemanticTokensLegend
            (LSP.List LSP.knownSemanticTokenTypes)
            (LSP.List LSP.knownSemanticTokenModifiers)
      maybeLoadedProgram <- dumpProgram filePath
      case maybeLoadedProgram of
        Nothing -> responder (Left $ LSP.ResponseError LSP.ServerNotInitialized "Please reload before requesting semantic tokens." Nothing)
        Just loadedProgram -> do
          case LSP.makeSemanticTokens legend $ highlightingInfos loadedProgram of
            Left errorMessage    -> responder (Left $ LSP.ResponseError LSP.InternalError errorMessage Nothing)
            Right semanticTokens -> responder (Right $ Just semanticTokens)

      -- stage <- load
      -- let
      --   highlightings = case stage of
      --     Raw    _      -> []
      --     Parsed result -> parsedHighlighings result
      --     Converted result ->
      --       parsedHighlighings (convertedPreviousStage result)
      --     TypeChecked result -> parsedHighlighings
      --       (convertedPreviousStage (typeCheckedPreviousStage result))
      --     Swept result -> parsedHighlighings
      --       (convertedPreviousStage
      --         (typeCheckedPreviousStage (sweptPreviousStage result))
      --       )
      -- let tokens = LSP.makeSemanticTokens legend highlightings
      -- case tokens of
      --   Left t -> return $ Left $ LSP.ResponseError LSP.InternalError t Nothing
      --   Right tokens' -> return $ Right $ Just tokens'