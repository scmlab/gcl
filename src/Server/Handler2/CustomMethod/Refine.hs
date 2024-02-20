{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Server.Handler2.CustomMethod.Refine (slowHandler) where

import Data.Loc.Range (Range (..), rangeFile)

import Server.Monad (ServerM, LoadedProgram (..))
import Server.Handler2.Utils
import Server.Handler2.CustomMethod.Utils
import Server.Handler2.CustomMethod.Reload as Reload
import Data.Text (Text)
import Server.CustomMethod (ResKind)
import Error (Error)

slowHandler :: Range -> Text -> ([ResKind] -> ServerM ()) -> (Error -> ServerM ()) -> ServerM ()
slowHandler specRange filledText onFinish onError = do
  -- remove brackets
  editText specRange filledText do
    -- reload source
    let filePath :: FilePath = rangeFile specRange
    Reload.handler filePath onFinish onError

-- 檢查 text typecheck 過了的話
-- 更新 loadedProgram (concrete/ abstract/ pos/ specs ...etc)
-- (會比重新load有效率嗎？laziness?
-- 用新的loadedProgram更新view

-- handler :: Range -> Text -> ([ResKind] -> ServerM ()) -> (Error -> ServerM ()) -> ServerM ()
-- handler specRange filledText onFinish onError = do
--   let filePath :: FilePath = rangeFile specRange
--   maybeLoadedProgram <- dumpProgram filePath
--   case maybeLoadedProgram of
--     Nothing -> _
--     Just loadedProgram -> do
--       let (refinedProgram, newRangeToFocus) = refine loadedProgram specRange filledText
--       _ <- cacheProgram filePath refinedProgram
--       response <- sendDiagnosticsAndMakeResponseFromLoadedProgram filePath refinedProgram newRangeToFocus
--       onFinish response

-- refine :: LoadedProgram -> Range -> Text -> (LoadedProgram, Maybe Range)
-- refine loadedProgram specRange filledText =
--   ( LoadedProgram
--       { _concreteProgram   = _
--       , _highlightingInfos = _
--       , _abstractProgram   = _
--       , _scopingInfo       = _
--       , _typeCheckingInfo  = _
--       , _proofObligations  = _
--       , _specifiations     = _
--       , _warnings          = _
--       , _redexes           = _
--       , _variableCounter   = _
--       }
--   , _
--   )