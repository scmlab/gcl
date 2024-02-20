
module Server.Handler2.CustomMethod.Refine (slowHandler) where

import Data.Loc.Range (Range (..), rangeFile)

import Server.Monad (ServerM)
import Server.Handler2.Utils
import Server.Handler2.CustomMethod.Utils
import Server.Handler2.CustomMethod.Reload as Reload

slowHandler :: Range -> Text -> ([ResKind] -> ServerM ()) -> (Error -> ServerM ()) -> ServerM ()
slowHandler specRange filledText onFinish onError = do
  -- remove brackets
  editText specRange filledText do
    -- reload source
    Reload.handler filepath onFinsih onError

-- 檢查 text typecheck 過了的話
-- 更新 loadedProgram (concrete/ abstract/ pos/ specs ...etc)
-- (會比重新load有效率嗎？laziness?
-- 用新的loadedProgram更新view

handler :: Range -> Text -> ServerM ()
handler specRange filledText = do
  let filepath :: FilePath = rangeFile specRange
  maybeLoadedProgram <- dumpProgram filepath
  case maybeLoadedProgram of
    Nothing -> _
    Just loadedProgram -> do
      let (refinedProgram, newRangeToFocus) = refine loadedProgram specRange filledText
      _ <- cacheProgram filePath refinedProgram
      response <- sendDiagnosticsAndMakeResponseFromLoadedProgram loadedProgam newRangeToFocus
      onFinish response

refine :: LoadedProgram -> Range -> Text -> (LoadedProgram, Maybe Range)
refine loadedProgram specRange filledText =
  LoadedProgram
  { _concreteProgram   = _
  , _highlightingInfos = _
  , _abstractProgram   = _
  , _scopingInfo       = _
  , _typeCheckingInfo  = _
  , _proofObligations  = _
  , _specifiations     = _
  , _warnings          = _
  , _redexes           = _
  , _variableCounter   = _
  }