{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE OverloadedStrings #-}

module Server.Interpreter.RealWorld
  ( ServerM,
    GlobalEnv (globalChan),
    initGlobalEnv,
    runServerM,
    logText, logStuff,
    customRequestResponder,
    notificationResponder,
    interpret,
    getMute
  )
where

import Control.Concurrent (Chan, newChan, writeChan)
import Control.Monad.Reader
import Control.Monad.Trans.Free
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
import Server.DSL (runCmdM, CmdM, Cmd(..), Result)
import Server.Diagnostic ()
import qualified Server.DSL as DSL
import Data.Loc.Range (Range)
import Pretty (toText)
import qualified Server.Util as J
import Server.Stab (collect)
import qualified Data.Aeson as JSON

--------------------------------------------------------------------------------

-- | State shared by all clients and requests
data GlobalEnv = GlobalEnv
  { -- Channel for printint log
    globalChan :: Chan Text,
    -- Keep tracks of all text selections (including cursor position)
    globalSelectionMap :: IORef (Map FilePath (Maybe Range)),
    -- Counter for generating fresh numbers
    globalCounter :: IORef Int,
    -- 
    globalMute :: IORef Bool,

    globalCachedResult :: IORef Result
  }

-- | Constructs an initial global state
initGlobalEnv :: IO GlobalEnv
initGlobalEnv = GlobalEnv <$> newChan <*> newIORef Map.empty <*> newIORef 0 <*> newIORef False <*> newIORef (Right ([], [], [], []))

--------------------------------------------------------------------------------

type ServerM = LspT () (ReaderT GlobalEnv IO)

runServerM :: GlobalEnv -> LanguageContextEnv () -> ServerM a -> IO a
runServerM env ctxEnv program = runReaderT (runLspT ctxEnv program) env



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

handleErrors :: FilePath -> Either [Error] [ResKind] -> ServerM [ResKind]
handleErrors filepath (Left errors) = do 
  version <- bumpVersionM
  let responses = [ResDisplay version (headerE "Errors" : map renderBlock errors), ResUpdateSpecs []]
  let diagnostics = errors >>= collect
  -- send diagnostics
  sendDiagnostics filepath diagnostics
  return responses
handleErrors _ (Right responses) = return responses

customRequestResponder :: FilePath -> (Response -> ServerM ()) -> Either [Error] [ResKind] -> ServerM ()
customRequestResponder filepath responder result = do  
  responses <- handleErrors filepath result 
  responder (Res filepath responses)

notificationResponder :: FilePath -> Either [Error] [ResKind] -> ServerM ()
notificationResponder filepath result = do
  responses <- handleErrors filepath result
  -- send responses
  sendNotification (SCustomMethod "guabao") $ JSON.toJSON $ Res filepath responses

--------------------------------------------------------------------------------

bumpVersionM :: ServerM Int
bumpVersionM = do
  ref <- lift $ asks globalCounter
  n <- liftIO $ readIORef ref
  liftIO $ writeIORef ref (succ n)
  return n

getMute :: ServerM Bool
getMute = do
  ref <- lift $ asks globalMute
  liftIO $ readIORef ref

setMute :: Bool -> ServerM ()
setMute b = do
  ref <- lift $ asks globalMute
  liftIO $ writeIORef ref b

cacheResult :: Result -> ServerM ()
cacheResult result = do
  ref <- lift $ asks globalCachedResult
  liftIO $ writeIORef ref result

readCachedResult :: ServerM Result
readCachedResult = do
  ref <- lift $ asks globalCachedResult
  liftIO $ readIORef ref

--------------------------------------------------------------------------------

interpret :: Show a => FilePath -> (Either [Error] a -> ServerM ()) -> CmdM a -> ServerM ()
interpret filepath responder p = case runCmdM p of
  Right (Pure responses) -> do
    logText $ " ### SendResponses " <> toText (show responses)
    -- send responses
    responder (Right responses)
    -- sendResponses filepath responder responses
  Right (Free (EditText range text next)) -> do
    logText $ " ### EditText " <> toText range <> " " <> text
    -- apply edit
    let removeSpec = TextEdit (J.toRange range) text
    let identifier = VersionedTextDocumentIdentifier (filePathToUri filepath) (Just 0)
    let textDocumentEdit = TextDocumentEdit identifier (List [InL removeSpec])
    let change = InL textDocumentEdit
    let workspaceEdit = WorkspaceEdit Nothing (Just (List [change])) Nothing
    let applyWorkspaceEditParams = ApplyWorkspaceEditParams (Just "Resolve Spec") workspaceEdit
    let callback _ = do
          interpret filepath responder $ do
            DSL.getSource >>= next

    void $ sendRequest SWorkspaceApplyEdit applyWorkspaceEditParams callback
  Right (Free (Mute b next)) -> do
    setMute b
    interpret filepath responder next
  Right (Free (GetFilePath next)) -> interpret filepath responder (next filepath)
  Right (Free (GetSource next)) -> do
    result <- fmap VFS.virtualFileText <$> getVirtualFile (toNormalizedUri (filePathToUri filepath))
    case result of
      Nothing -> responder (Left [CannotReadFile filepath])
      Just source -> interpret filepath responder (next source)
  Right (Free (GetLastSelection next)) -> do
    ref <- lift $ asks globalSelectionMap
    mapping <- liftIO $ readIORef ref
    let selection = join $ Map.lookup filepath mapping
    interpret filepath responder (next selection)
  Right (Free (PutLastSelection selection next)) -> do
    ref <- lift $ asks globalSelectionMap
    liftIO $ modifyIORef' ref (Map.insert filepath (Just selection))
    interpret filepath responder next
  Right (Free (ReadCachedResult next)) -> do
    result <- readCachedResult
    interpret filepath responder (next result)
  Right (Free (CacheResult result next)) -> do
    cacheResult result
    interpret filepath responder next
  Right (Free (BumpResponseVersion next)) -> do
    n <- bumpVersionM
    interpret filepath responder (next n)
  Right (Free (Log text next)) -> do
    logText text
    interpret filepath responder next
  Right (Free (SendDiagnostics diagnostics next)) -> do
    -- send diagnostics
    sendDiagnostics filepath diagnostics
    interpret filepath responder next
  Left errors -> do
    setMute False -- unmute on error!
    cacheResult (Left errors)
    logStuff errors

    -- responses <- errorsToResponses filepath errors
    responder (Left errors)

    -- sendNotificationsPrim filepath (Left errors)
    return ()
