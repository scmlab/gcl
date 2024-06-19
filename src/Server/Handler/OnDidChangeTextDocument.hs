{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Server.Handler.OnDidChangeTextDocument where

import Server.Monad (ServerM, FileState (..), modifyFileState, sendUpdateSpecNotification, Versioned)
import GCL.Predicate (Spec(..))
import Server.PositionMapping (mkDelta, applyChange, toCurrentRange', PositionDelta)
import qualified Language.LSP.Types as LSP
import qualified Server.SrcLoc as SrcLoc
import qualified Data.Map as Map
import Data.Loc.Range (Range)
import Server.SrcLoc (ToOffset(ToOffset))

handler :: FilePath -> [LSP.TextDocumentContentChangeEvent] -> ServerM ()
handler filePath changes = do
  modifyFileState filePath (\filesState@FileState{positionDelta, editedVersion, specifications, proofObligations} ->
    filesState
      { positionDelta = foldl applyChange positionDelta changes
      , editedVersion = editedVersion + 1
      , specifications = translateSpecsRangeThroughOneVersion editedVersion changes filePath specifications
      -- TODO: update proofObligations range 
      }
    )

  -- send notification to update Specs
  sendUpdateSpecNotification filePath
  -- TODO: send notification to update POs

translateSpecsRangeThroughOneVersion
  :: Int -> [LSP.TextDocumentContentChangeEvent] -> FilePath -> [Versioned Spec] -> [Versioned Spec]
translateSpecsRangeThroughOneVersion fromVersion changes filePath specs = do
  (version, spec) <- specs
  if fromVersion == version then
    case translateSpecRange spec of
      Nothing -> []
      Just spec' -> [(version + 1, spec')]
  else if fromVersion + 1 == version then
    [(version, spec)]
  else error "should not happen"
  where
    translateSpecRange :: Spec -> Maybe Spec
    translateSpecRange spec = do
      let delta :: PositionDelta = mkDelta changes
      newRange <- currentSpecRange filePath delta spec
      return $ spec {specRange = newRange}

currentSpecRange :: FilePath -> PositionDelta -> Spec -> Maybe Range
currentSpecRange filePath delta Specification{specRange = oldRange} = do
  let oldLspRange :: LSP.Range = SrcLoc.toLSPRange oldRange
  currentLspRange :: LSP.Range <- toCurrentRange' delta oldLspRange
  return $ SrcLoc.fromLSPRangeWithoutCharacterOffset filePath currentLspRange