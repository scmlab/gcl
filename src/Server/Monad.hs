{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE ScopedTypeVariables #-}

{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

module Server.Monad where

import Data.Text
import qualified Data.Map as Map
import           Control.Concurrent             ( Chan
                                                , newChan
                                                , writeChan
                                                )
import           Control.Monad.Reader
import           Data.IORef                     ( IORef
                                                , modifyIORef
                                                , readIORef
                                                , newIORef
                                                )
import           Data.Map                       ( Map )
import qualified Language.LSP.Types             as LSP
import qualified Language.LSP.Server            as LSP
import qualified Language.LSP.VFS               as LSP
import qualified Language.LSP.Diagnostics       as LSP
import qualified Data.Aeson                     as JSON
import GCL.Predicate (Spec (Specification, specID), PO)
import qualified Syntax.Abstract as Abstract
import qualified Syntax.Concrete as Concrete
import qualified Syntax.Typed    as Typed
import Server.IntervalMap (IntervalMap)
import Server.PositionMapping (PositionDelta)
import Data.Loc.Range (Range, rangeStart)
import qualified Server.SrcLoc                 as SrcLoc
import qualified Data.Text as Text
import Data.Loc (posCol)
import Data.Version (Version(Version))
import GCL.WP.Types (StructWarning)
import Language.LSP.Types.Lens (HasMessage(message))

-- | State shared by all clients and requests
data GlobalState = GlobalState
  { logChannel  :: Chan Text   -- Channel for printing log
  , filesState :: IORef (Map FilePath FileState)
  }

type Versioned a = (Int, a)

data FileState = FileState
    -- main states for Reload and Refine
  { refinedVersion   :: Int  -- the version number of the last refine
  , specifications   :: [Versioned Spec] -- editedVersion or (editedVersion + 1)
  , proofObligations :: [Versioned PO] -- editedVersion
  , warnings         :: [Versioned StructWarning]

  -- to support other LSP methods in a light-weighted manner
  , loadedVersion    :: Int  -- the version number of the last reload
  , toOffsetMap      :: SrcLoc.ToOffset
  , concrete         :: Concrete.Program
  , semanticTokens   :: [LSP.SemanticTokenAbsolute]
  , abstract         :: Abstract.Program
  , idCount          :: Int
  , definitionLinks  :: IntervalMap LSP.LocationLink
  , hoverInfos       :: IntervalMap LSP.Hover
  , elaborated       :: Typed.Program
  , positionDelta    :: PositionDelta   -- loadedVersion ~> editedVersion
  , editedVersion    :: Int  -- the version number of the last change
  }

-- | Constructs an initial global state
initGlobalEnv :: IO GlobalState
initGlobalEnv =
  GlobalState
    <$> newChan
    <*> newIORef Map.empty

--------------------------------------------------------------------------------

type ServerM = LSP.LspT () (ReaderT GlobalState IO)

runServerM :: GlobalState -> LSP.LanguageContextEnv () -> ServerM a -> IO a
runServerM globalState ctxEnv program = runReaderT (LSP.runLspT ctxEnv program) globalState

--------------------------------------------------------------------------------
-- | Helper functions for side effects

-- display Text
logText :: Text -> ServerM ()
logText s = do
  chan <- lift $ asks logChannel
  liftIO $ writeChan chan s

loadFileState :: FilePath -> ServerM (Maybe FileState)
loadFileState filePath = do
  logText "ask file state ref\n"
  fileStateRef <- lift $ asks filesState
  logText "ask file state map\n"
  fileStateMap <- liftIO $ readIORef fileStateRef
  logText "lookup file state map\n"
  case Map.lookup filePath fileStateMap of
    Nothing               -> do
      logText "  not found\n"
      return Nothing
    Just loadedFileState -> do
      logText "  found\n"
      return $ Just loadedFileState

saveFileState :: FilePath -> FileState -> ServerM ()
saveFileState filePath fileState = do
  fileStateRef <- lift $ asks filesState
  liftIO $ modifyIORef fileStateRef (Map.insert filePath fileState)

logFileState :: Show a => FilePath -> (FileState -> a) -> ServerM ()
logFileState filePath f = do
  logText "====== "
  logText . Text.pack $ filePath
  logText " ======\n"
  logText "\n"
  maybeFileState <- loadFileState filePath
  case maybeFileState of
    Nothing -> logText "not loaded yet\n"
    Just fileState -> do
      logText "loaded\n"
      logText . Text.pack . show $ f fileState
      logText "\n"
  logText "=======================\n"

modifyFileState :: FilePath -> (FileState -> FileState) -> ServerM ()
modifyFileState filePath modifier = do
  maybeFileState <- loadFileState filePath
  case maybeFileState of
    Nothing -> return ()
    Just fileState -> do
      let fileState' = modifier fileState
      saveFileState filePath fileState'

bumpVersion :: FilePath -> ServerM ()
bumpVersion filePath = do
  modifyFileState filePath (\fileState@FileState{editedVersion} -> fileState {editedVersion = editedVersion + 1})

updateIdCounter :: FilePath -> 
  Int -> ServerM ()
updateIdCounter filePath count = do
  modifyFileState filePath (\fileState -> fileState {idCount = count})

saveEditedVersion :: FilePath -> Int -> ServerM ()
saveEditedVersion filePath version = do
  modifyFileState filePath (\fileState -> fileState {editedVersion = version})

pushSpecs :: Int -> FilePath -> [Spec] -> ServerM ()
pushSpecs version filePath newSpecs = do
  let newVersionedSpecs :: [Versioned Spec] = Prelude.map (\spec -> (version, spec)) newSpecs
  modifyFileState filePath (\fileState@FileState{specifications} ->
    fileState{specifications = specifications ++ newVersionedSpecs})

pushPos :: Int -> FilePath -> [PO] -> ServerM ()
pushPos version filePath newPos = do
  let newVersionedPos :: [Versioned PO] = Prelude.map (\po -> (version, po)) newPos
  modifyFileState filePath (\fileState@FileState{proofObligations} ->
    fileState{proofObligations = proofObligations ++ newVersionedPos})

pushWarnings :: Int -> FilePath -> [StructWarning] -> ServerM ()
pushWarnings version filePath newWarnings = do
  let newVersionedWarnings :: [Versioned StructWarning] = Prelude.map (\warning -> (version, warning)) newWarnings
  modifyFileState filePath (\fileState@FileState{warnings} ->
    fileState{warnings = warnings ++ newVersionedWarnings})


deleteSpec :: FilePath -> Spec -> ServerM ()
deleteSpec filePath Specification{specID = targetSpecId} = do
  modifyFileState filePath (\filesState@FileState{specifications} ->
    filesState{specifications = Prelude.filter (\(_, Specification{specID}) -> specID /= targetSpecId) specifications})

readSource :: FilePath -> ServerM (Maybe Text)
readSource filepath = fmap LSP.virtualFileText
                      <$> LSP.getVirtualFile (LSP.toNormalizedUri (LSP.filePathToUri filepath))

modifyPositionDelta :: FilePath -> (PositionDelta -> PositionDelta) -> ServerM ()
modifyPositionDelta filePath modifier = do
  modifyFileState filePath (\fileState@FileState{positionDelta} -> fileState{positionDelta = modifier positionDelta})


editTexts :: FilePath -> [(Range, Text)] -> ServerM () -> ServerM ()
editTexts filepath rangeTextPairs onSuccess = do
  let requestParams :: LSP.ApplyWorkspaceEditParams
        = LSP.ApplyWorkspaceEditParams {
            _label = Just "Resolve Spec",
            _edit = LSP.WorkspaceEdit {
              _changes = Nothing,
              _documentChanges = Just (LSP.List [LSP.InL textDocumentEdit]),
              _changeAnnotations = Nothing
            }
          }
  _requestId <- LSP.sendRequest LSP.SWorkspaceApplyEdit requestParams (\_ -> onSuccess)
  return ()

  where
    textDocumentEdit :: LSP.TextDocumentEdit
    textDocumentEdit = LSP.TextDocumentEdit {
      _textDocument = LSP.VersionedTextDocumentIdentifier (LSP.filePathToUri filepath) (Just 0),
      _edits = LSP.List (Prelude.map LSP.InL textEdits)
    }
    textEdits :: [LSP.TextEdit]
    textEdits = Prelude.map makeTextEdit rangeTextPairs
    makeTextEdit :: (Range, Text) -> LSP.TextEdit
    makeTextEdit (range, textToReplace) = LSP.TextEdit {
      _range = SrcLoc.toLSPRange range,
      _newText = textToReplace
    }

sendCustomNotification :: Text -> JSON.Value -> ServerM ()
sendCustomNotification methodId json = LSP.sendNotification (LSP.SCustomMethod methodId) json


-- send diagnostics
-- NOTE: existing diagnostics would be erased if `diagnostics` is empty
sendDiagnostics :: FilePath -> [LSP.Diagnostic] -> ServerM ()
sendDiagnostics filePath diagnostics = do
  maybeFileState <- loadFileState filePath
  let maybeVersion = fmap editedVersion maybeFileState
  LSP.publishDiagnostics 100
                       (LSP.toNormalizedUri (LSP.filePathToUri filePath))
                       maybeVersion
                       (LSP.partitionBySource diagnostics)

digHoles :: FilePath -> [Range] -> ServerM () -> ServerM ()
digHoles filePath ranges onFinish = do
  -- logText $ "    < DigHoles " <> (map ranges toText)
  let indent range = Text.replicate (posCol (rangeStart range) - 1) " "
  let diggedText range = "[!\n" <> indent range <> "\n" <> indent range <> "!]"
  editTexts filePath (Prelude.map (\range -> (range, diggedText range)) ranges) onFinish

sendDebugMessage :: Text -> ServerM ()
sendDebugMessage message = do
  let requestParams =
        LSP.ShowMessageRequestParams
          LSP.MtInfo
          message
          Nothing
  LSP.sendRequest LSP.SWindowShowMessageRequest requestParams (\_ -> return ())
  return ()
