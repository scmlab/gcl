{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE DuplicateRecordFields #-}

module Server.Handler.SemanticTokens where

import Server.Monad (ServerM, FileState (..), loadFileState, logText)
import qualified Language.LSP.Types as LSP
import Language.LSP.Types (SemanticTokenAbsolute(..), Position(..))
import Server.PositionMapping (PositionDelta(..), PositionResult(..))

handler :: LSP.Uri -> (Either LSP.ResponseError (Maybe LSP.SemanticTokens) -> ServerM ()) -> ServerM ()
handler fileUri responder = do
  logText "semantic token: start\n"
  case LSP.uriToFilePath fileUri of
    Nothing       -> respondError (LSP.ResponseError LSP.InvalidParams "Invalid uri" Nothing)
    Just filePath -> do
      maybeFileState <- loadFileState filePath
      case maybeFileState of
        Nothing                          -> respondError (LSP.ResponseError LSP.ServerNotInitialized "Please reload before requesting for semantic tokens." Nothing)
        Just (FileState{semanticTokens = oldSemanticTokenAbsolutes, positionDelta})
                                         -> do
          let newSemanticTokenAbsolutes = translatePositions positionDelta oldSemanticTokenAbsolutes
          let legend = LSP.SemanticTokensLegend
                        (LSP.List LSP.knownSemanticTokenTypes)
                        (LSP.List LSP.knownSemanticTokenModifiers)
          case LSP.makeSemanticTokens legend newSemanticTokenAbsolutes of
            Left _errorMessage   -> respondError (LSP.ResponseError LSP.InternalError _errorMessage Nothing)
            Right semanticTokens -> respondResult semanticTokens
  where
    respondResult :: LSP.SemanticTokens -> ServerM ()
    respondResult result = responder (Right $ Just result)
    respondError :: LSP.ResponseError -> ServerM ()
    respondError err = responder (Left err)

translatePosition :: PositionDelta -> LSP.SemanticTokenAbsolute -> Maybe LSP.SemanticTokenAbsolute
translatePosition PositionDelta{toDelta} oldToken@(LSP.SemanticTokenAbsolute{line, startChar}) =
  case toDelta oldPosition of
    PositionExact (Position{_line, _character}) -> Just (oldToken{line = _line, startChar = _character})
    _                                           -> Nothing
  where
    oldPosition :: LSP.Position
    oldPosition = LSP.Position line startChar

translatePositions :: PositionDelta -> [LSP.SemanticTokenAbsolute] -> [LSP.SemanticTokenAbsolute]
translatePositions positionDelta oldTokens = do
  oldToken <- oldTokens
  case translatePosition positionDelta oldToken of
    Nothing       -> []
    Just newToken -> return newToken