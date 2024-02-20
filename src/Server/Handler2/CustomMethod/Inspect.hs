{-# LANGUAGE ScopedTypeVariables #-}
module Server.Handler2.CustomMethod.Inspect (handler) where

import Data.Loc.Range (Range (..), rangeFile)

import Server.Handler2.Utils (dumpProgram)
import Server.Handler2.CustomMethod.Utils (sendDiagnosticsAndMakeResponseFromLoadedProgram)

handler :: Range -> ([ResKind] -> ServerM ()) -> (Error -> ServerM ()) -> ServerM ()
handler rangeToInspect onFinish onError = do
  let filePath :: FilePath = rangeFile range
  maybeLoadedProgram <- dumpProgram filePath
  case maybeLoadedProgram of
    Nothing -> onError (Others "Please reload before inspect.")
    Just loadedProgam -> do
      response <- sendDiagnosticsAndMakeResponseFromLoadedProgram loadedProgam rangeToInspect
      onFinish response

