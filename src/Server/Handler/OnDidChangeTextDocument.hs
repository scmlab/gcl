{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Server.Handler.OnDidChangeTextDocument where

import Server.Monad (ServerM, FileState (..), modifyFileState, Versioned, logText, logFileState)
import Server.Notification.Update (sendUpdateNotification)
import GCL.Predicate (Spec(..), PO (..), Origin (..))
import Server.PositionMapping (mkDelta, applyChange, toCurrentRange', PositionDelta)
import qualified Language.LSP.Types as LSP
import qualified Server.SrcLoc as SrcLoc
import Data.Loc.Range (Range (..), fromLoc)
import Data.Loc (Loc (..), Located (..))
import GCL.WP.Types (StructWarning (MissingBound))

handler :: FilePath -> [LSP.TextDocumentContentChangeEvent] -> ServerM ()
handler filePath changes = do
  modifyFileState filePath (\filesState@FileState{positionDelta, editedVersion, specifications, proofObligations, warnings} ->
    filesState
      { positionDelta = foldl applyChange positionDelta changes
      , editedVersion = editedVersion + 1
      , specifications = translateThroughOneVersion translateSpecRange editedVersion specifications
      , proofObligations = translateThroughOneVersion translatePoRange editedVersion proofObligations
      , warnings = translateThroughOneVersion translateWarningRange editedVersion warnings
      }
    )
  logFileState filePath (map (\(version, Specification{specRange}) -> (version, specRange)) . specifications)

  -- send notification to update Specs and POs
  logText "didChange: fileState modified\n"
  sendUpdateNotification filePath
  logText "didChange: upate notification sent\n"
  where
    translateThroughOneVersion
      :: (FilePath -> PositionDelta -> a -> Maybe a)
      -> Int -> [Versioned a] -> [Versioned a]
    translateThroughOneVersion translator fromVersion versioned = do
      (version, a) <- versioned
      let delta :: PositionDelta = mkDelta changes
      if fromVersion == version then
        case translator filePath delta a of
          Nothing -> []
          Just spec' -> [(version + 1, spec')]
      else if fromVersion + 1 == version then
        [(version, a)]
      else error "should not happen"

-- 目前只維護 specRange，而沒有更新 specPre 和 specPost 裡面的位置資訊
-- 如果未來前端有需要的話，請在這裡維護
translateSpecRange :: FilePath -> PositionDelta -> Spec -> Maybe Spec
translateSpecRange filePath delta spec@Specification{specRange = oldRange} = do
  let oldLspRange :: LSP.Range = SrcLoc.toLSPRange oldRange
  currentLspRange :: LSP.Range <- toCurrentRange' delta oldLspRange
  let newRange = SrcLoc.fromLSPRangeWithoutCharacterOffset filePath currentLspRange
  return $ spec {specRange = newRange}

-- 目前只維護 poOrigin 裡面的 location，而沒有更新 poPre 和 poPost 裡面的位置資訊
-- 如果未來前端有需要的話，請在這裡維護
translatePoRange :: FilePath -> PositionDelta -> PO -> Maybe PO
translatePoRange filePath delta po@PO{poOrigin} = do
  oldRange :: Range <- fromLoc (locOf poOrigin)
  let oldLspRange :: LSP.Range = SrcLoc.toLSPRange oldRange
  currentLspRange :: LSP.Range <- toCurrentRange' delta oldLspRange
  let _newRange@(Range x y) = SrcLoc.fromLSPRangeWithoutCharacterOffset filePath currentLspRange
  return $ po {poOrigin = setOriginLocation (Loc x y) poOrigin}

translateWarningRange :: FilePath -> PositionDelta -> StructWarning -> Maybe StructWarning
translateWarningRange filePath delta (MissingBound oldRange) = do
  let oldLspRange :: LSP.Range = SrcLoc.toLSPRange oldRange
  currentLspRange :: LSP.Range <- toCurrentRange' delta oldLspRange
  let newRange = SrcLoc.fromLSPRangeWithoutCharacterOffset filePath currentLspRange
  return $ MissingBound newRange

setOriginLocation :: Loc -> Origin -> Origin
setOriginLocation l (AtAbort       _) = AtAbort l
setOriginLocation l (AtSkip        _) = AtSkip l
setOriginLocation l (AtSpec        _) = AtSpec l
setOriginLocation l (AtAssignment  _) = AtAssignment l
setOriginLocation l (AtAssertion   _) = AtAssertion l
setOriginLocation l (AtIf          _) = AtIf l
setOriginLocation l (AtLoop        _) = AtLoop l
setOriginLocation l (AtTermination _) = AtTermination l
setOriginLocation l (Explain h e i p _) = Explain h e i p l
