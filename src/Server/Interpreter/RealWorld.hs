{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE OverloadedStrings #-}

module Server.Interpreter.RealWorld
  ( ServerM,
    GlobalEnv (globalChan),
    initGlobalEnv,
    runServerM,
    logText, logStuff,
    interpret
  )
where

import Control.Concurrent (Chan, newChan, writeChan)
import Control.Monad.Reader
import Control.Monad.Trans.Free
import qualified Data.Aeson as JSON
import Data.IORef (IORef, modifyIORef', newIORef, readIORef, writeIORef)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Text (Text)
import qualified Data.Text as Text
import Error
import Language.LSP.Diagnostics
import Language.LSP.Server
import Language.LSP.Types hiding (TextDocumentSyncClientCapabilities (..), Range(..))
import qualified Language.LSP.VFS as VFS
import Render
import Server.CustomMethod
import Server.DSL (runCmdM, CmdM, Cmd(..))
import Server.Diagnostic
import qualified Server.DSL as DSL
import Data.Loc.Range (Range)

--------------------------------------------------------------------------------

-- | State shared by all clients and requests
data GlobalEnv = GlobalEnv
  { -- Channel for printint log
    globalChan :: Chan Text,
    -- Keep tracks of all text selections (including cursor position)
    globalSelectionMap :: IORef (Map FilePath (Maybe Range)),
    -- Counter for generating fresh numbers
    globalCounter :: IORef Int
  }

-- | Constructs an initial global state
initGlobalEnv :: IO GlobalEnv
initGlobalEnv = GlobalEnv <$> newChan <*> newIORef Map.empty <*> newIORef 0

--------------------------------------------------------------------------------

type ServerM = LspT () (ReaderT GlobalEnv IO)

runServerM :: GlobalEnv -> LanguageContextEnv () -> ServerM a -> IO a
runServerM env ctxEnv program = runReaderT (runLspT ctxEnv program) env

type Responder = Response -> ServerM ()

--------------------------------------------------------------------------------

-- | Logging instances of Show
logStuff :: Show a => a -> ServerM ()
logStuff x = do
  chan <- lift $ asks globalChan
  liftIO $ writeChan chan (Text.pack (show x))

-- | Logging Text
logText :: Text -> ServerM ()
logText s = do
  chan <- lift $ asks globalChan
  liftIO $ writeChan chan s

--------------------------------------------------------------------------------

sendDiagnostics :: FilePath -> [Diagnostic] -> ServerM ()
sendDiagnostics filepath diagnostics = do
  -- send diagnostics
  ref <- lift $ asks globalCounter
  version <- liftIO $ readIORef ref
  liftIO $ writeIORef ref (succ version)
  publishDiagnostics 100 (toNormalizedUri (filePathToUri filepath)) (Just version) (partitionBySource diagnostics)

sendResponses :: FilePath -> Maybe Responder -> [ResKind] -> ServerM ()
sendResponses filepath responder responses = do
  -- send responses
  case responder of
    Nothing -> sendNotification (SCustomMethod "guacamole") $ JSON.toJSON $ Res filepath responses
    Just f -> f $ Res filepath responses

handleErrors :: FilePath -> Maybe Responder -> [Error] -> ServerM ()
handleErrors filepath responder errors = do
  version <- bumpVersionM
  -- (IdInt version)
  let responses = [ResDisplay version (headerE "Errors" : map renderBlock errors), ResUpdateSpecs []]
  let diagnostics = errors >>= toDiagnostics
  -- send diagnostics
  sendDiagnostics filepath diagnostics
  -- send responses
  sendResponses filepath responder responses

bumpVersionM :: ServerM Int
bumpVersionM = do
  ref <- lift $ asks globalCounter
  n <- liftIO $ readIORef ref
  liftIO $ writeIORef ref (succ n)
  return n

--------------------------------------------------------------------------------

interpret :: FilePath -> Maybe Responder -> CmdM () -> ServerM ()
interpret filepath responder p = case runCmdM p of
  Right (Pure ()) -> logText "Improper termination"
  Right (Free (EditText range text next)) -> do
    logText "Before EditText"
    -- apply edit
    let removeSpec = TextEdit (rangeToRange range) text
    let identifier = VersionedTextDocumentIdentifier (filePathToUri filepath) (Just 0)
    let textDocumentEdit = TextDocumentEdit identifier (List [InL removeSpec])
    let change = InL textDocumentEdit
    let workspaceEdit = WorkspaceEdit Nothing (Just (List [change])) Nothing
    let applyWorkspaceEditParams = ApplyWorkspaceEditParams (Just "Resolve Spec") workspaceEdit
    let callback _ = do
          logText "After EditText"
          interpret filepath responder $ do
            DSL.getSource >>= next 

    void $ sendRequest SWorkspaceApplyEdit applyWorkspaceEditParams callback
  Right (Free (GetFilePath next)) -> interpret filepath responder (next filepath)
  Right (Free (GetSource next)) -> do
    result <- fmap VFS.virtualFileText <$> getVirtualFile (toNormalizedUri (filePathToUri filepath))
    case result of
      Nothing -> handleErrors filepath responder [CannotReadFile filepath]
      Just source -> do
        logText "GetSource"
        interpret filepath responder (next source)
  Right (Free (GetLastSelection next)) -> do
    ref <- lift $ asks globalSelectionMap
    mapping <- liftIO $ readIORef ref
    let selection = join $ Map.lookup filepath mapping
    interpret filepath responder (next selection)
  Right (Free (PutLastSelection selection next)) -> do
    ref <- lift $ asks globalSelectionMap
    liftIO $ modifyIORef' ref (Map.update (\_ -> Just (Just selection)) filepath)
    interpret filepath responder next
  Right (Free (BumpResponseVersion next)) -> do
    n <- bumpVersionM
    interpret filepath responder (next n)
  Right (Free (Log text next)) -> do
    logText text
    interpret filepath responder next
  Right (Free (Terminate responses diagnostics)) -> do
    -- send diagnostics
    sendDiagnostics filepath diagnostics
    -- send responses
    sendResponses filepath responder responses
  Left errors -> do
    logStuff errors
    handleErrors filepath responder errors
