{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DeriveGeneric #-}

module Server.Handler.Guabao.Refine where

import qualified Data.Aeson.Types as JSON
import GHC.Generics ( Generic )
import Server.Monad (ServerM, FileState(..), loadFileState)
import Error (Error)
import GCL.Predicate (Spec(..), PO)
import Server.Load (load)
import Data.Loc.Range (Range)
import Server.PositionMapping (fromCurrentRange, PositionDelta(..), PositionMapping(..))
import Server.SrcLoc (fromLSPRange, toLSPRange)
import qualified Language.LSP.Types as LSP
import qualified Server.SrcLoc as SrcLoc
import Data.Text (Text)
import Data.List (find)
import qualified Syntax.Concrete as C
import qualified Syntax.Abstract as A

data RefineParams = RefineParams
  { filePath     :: FilePath
  , range        :: Range
  , fragmentText :: Text
  }
  deriving (Eq, Show, Generic)

instance JSON.FromJSON RefineParams

data RefineResult = RefineResult
  { specifications :: [Spec]
  , proofObligations :: [PO]
  }
  deriving (Eq, Show, Generic)
instance JSON.ToJSON RefineResult

-- TODO customize refine error
type RefineError = Error

handler :: RefineParams -> (RefineResult -> ServerM ()) -> (RefineError -> ServerM ()) -> ServerM ()
handler params@RefineParams{filePath, range, fragmentText} onSuccess onError = do
  maybeFileState <- loadFileState filePath
  case maybeFileState of
    Nothing -> return () -- TODO: report error using onError
    Just fileState -> do
      let FileState{
        hasChangedOutsideSpecsSinceLastReload,
        positionDelta,
        toOffsetMap,
        specifications
      } = fileState
      if hasChangedOutsideSpecsSinceLastReload
        then load filePath (\_ -> handler params onSuccess onError) onError
        else do
          -- calculate rangeAtLastReload with positionDelta
          let lspRange = SrcLoc.toLSPRange range
          case fromCurrentRange' positionDelta lspRange of
            Nothing -> return ()
            Just lspRangeAtLastReload -> do
              let rangeAtLastReload = SrcLoc.fromLSPRange toOffsetMap filePath lspRangeAtLastReload
              -- find the spec with rangeAtLastReload
              case findSpecWithRange rangeAtLastReload specifications of
                Nothing   -> return () -- TODO: error "spec not found at range"
                Just spec -> do
                  -- TODO
                  return ()
              return ()
          return ()

fromCurrentRange' :: PositionDelta -> LSP.Range -> Maybe LSP.Range
fromCurrentRange' = fromCurrentRange . PositionMapping

findSpecWithRange :: Range -> [Spec] -> Maybe Spec
findSpecWithRange range = find (\Specification{specRange} -> specRange == range)

deleteSpecWithRange :: Range -> [Spec] -> [Spec]
deleteSpecWithRange range = filter (\Specification{specRange} -> specRange == range)

parseFragment :: Text -> Maybe [C.Stmt]
parseFragment fragment = error "not yet implemented"

toAbstractFragment :: [C.Stmt] -> Maybe [A.Stmt]
toAbstractFragment fragment = error "not yet implemented"

-- eleborateFragment :: E.Program -> Range -> [A.Stmt] -> Maybe [E.Stmt]
-- eleborateFragment elaborated specRange fragment = error "TODO find type env. with specRange and elaborate fragment"

sweepFragment :: [A.Stmt] -> Maybe ([PO], [Spec])
sweepFragment fragment = error "TODO find new POs and specs with StructStmt"

