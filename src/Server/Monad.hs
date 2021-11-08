{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE OverloadedStrings #-}

module Server.Monad
  ( ServerM
  , GlobalEnv(globalChan)
  , initGlobalEnv
  , runServerM
  , logText
  , customRequestResponder
  , notificationResponder
  , interpret
  , getMute
  ) where

import           Control.Concurrent             ( Chan
                                                , newChan
                                                , writeChan
                                                )
import           Control.Monad.Reader
import           Control.Monad.Trans.Free
import qualified Data.Aeson                    as JSON
import           Data.IORef                     ( IORef
                                                , modifyIORef'
                                                , newIORef
                                                , readIORef
                                                , writeIORef
                                                )
import           Data.Loc.Range                 ( Range )
import           Data.Map                       ( Map )
import qualified Data.Map                      as Map
import           Data.Text                      ( Text )
import qualified Data.Text                     as Text
import           Error
import qualified Language.LSP.Diagnostics      as J
import qualified Language.LSP.Server           as J
import qualified Language.LSP.Types            as J
import qualified Language.LSP.VFS              as J
import           Pretty                         ( toText )
import           Render
import           Server.CustomMethod
import           Server.DSL                     ( Cmd(..)
                                                , CmdM
                                                , Result
                                                , runCmdM
                                                )
import qualified Server.DSL                    as DSL
import           Server.Handler.Diagnostic      ( collect )
import qualified Server.SrcLoc                 as SrcLoc

--------------------------------------------------------------------------------

-- | State shared by all clients and requests
data GlobalEnv = GlobalEnv
  { -- Channel for printing log
    globalChan         :: Chan Text
  ,
    -- Keep tracks of all text selections (including cursor position)
    globalSelectionMap :: IORef (Map FilePath (Maybe Range))
  ,
    -- Counter for generating fresh numbers
    globalCounter      :: IORef Int
  ,
    -- 
    globalMute         :: IORef Bool
  , globalCachedResult :: IORef (Map FilePath Result)
  }

-- | Constructs an initial global state
initGlobalEnv :: IO GlobalEnv
initGlobalEnv =
  GlobalEnv
    <$> newChan
    <*> newIORef Map.empty
    <*> newIORef 0
    <*> newIORef False
    <*> newIORef Map.empty

--------------------------------------------------------------------------------

type ServerM = J.LspT () (ReaderT GlobalEnv IO)

runServerM :: GlobalEnv -> J.LanguageContextEnv () -> ServerM a -> IO a
runServerM env ctxEnv program = runReaderT (J.runLspT ctxEnv program) env

--------------------------------------------------------------------------------

-- | Logging Text
logText :: Text -> ServerM ()
logText s = do
  chan <- lift $ asks globalChan
  liftIO $ writeChan chan s

--------------------------------------------------------------------------------

sendDiagnostics :: FilePath -> [J.Diagnostic] -> ServerM ()
sendDiagnostics filepath diagnostics = do
  -- send diagnostics
  version <- bumpVersionM
  J.publishDiagnostics 100
                       (J.toNormalizedUri (J.filePathToUri filepath))
                       (Just version)
                       (J.partitionBySource diagnostics)

handleErrors :: FilePath -> Either [Error] [ResKind] -> ServerM [ResKind]
handleErrors filepath (Left errors) = do
  version <- bumpVersionM
  let responses =
        [ResDisplay version (map renderSection errors), ResUpdateSpecs []]
  let diagnostics = errors >>= collect
  -- send diagnostics
  sendDiagnostics filepath diagnostics
  return responses
handleErrors _ (Right responses) = return responses

customRequestResponder
  :: FilePath
  -> (Response -> ServerM ())
  -> Either [Error] [ResKind]
  -> ServerM ()
customRequestResponder filepath responder result = do
  responses <- handleErrors filepath result
  responder (Res filepath responses)

notificationResponder :: J.Uri -> Either [Error] [ResKind] -> ServerM ()
notificationResponder uri result = case J.uriToFilePath uri of
  Nothing       -> pure ()
  Just filepath -> do
    responses <- handleErrors filepath result
    -- send responses
    J.sendNotification (J.SCustomMethod "guabao") $ JSON.toJSON $ Res
      filepath
      responses

--------------------------------------------------------------------------------

bumpVersionM :: ServerM Int
bumpVersionM = do
  ref <- lift $ asks globalCounter
  n   <- liftIO $ readIORef ref
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

cacheResult :: FilePath -> Result -> ServerM ()
cacheResult filepath result = do
  ref <- lift $ asks globalCachedResult
  liftIO $ modifyIORef' ref (Map.insert filepath result)

readCachedResult :: FilePath -> ServerM (Maybe Result)
readCachedResult filepath = do
  ref     <- lift $ asks globalCachedResult
  mapping <- liftIO $ readIORef ref
  return (Map.lookup filepath mapping)

--------------------------------------------------------------------------------

interpret
  :: Show a => J.Uri -> (Either [Error] a -> ServerM ()) -> CmdM a -> ServerM ()
interpret uri responder p = case J.uriToFilePath uri of
  Nothing       -> pure ()
  Just filepath -> case runCmdM p of
    Right (Pure responses) -> do
      -- send responses
      responder (Right responses)
      -- sendResponses filepath responder responses
    Right (Free (EditText range text next)) -> do
      logText $ " ### EditText " <> toText range <> " " <> text
      -- apply edit
      let removeSpec = J.TextEdit (SrcLoc.toLSPRange range) text
      let identifier = J.VersionedTextDocumentIdentifier uri (Just 0)
      let textDocumentEdit =
            J.TextDocumentEdit identifier (J.List [J.InL removeSpec])
      let change = J.InL textDocumentEdit
      let workspaceEdit =
            J.WorkspaceEdit Nothing (Just (J.List [change])) Nothing
      let applyWorkspaceEditParams =
            J.ApplyWorkspaceEditParams (Just "Resolve Spec") workspaceEdit
      let callback _ = interpret uri responder $ do
            DSL.getSource >>= next

      void $ J.sendRequest J.SWorkspaceApplyEdit
                           applyWorkspaceEditParams
                           callback
    Right (Free (SetMute b next)) -> do
      setMute b
      interpret uri responder next
    Right (Free (GetFilePath next)) -> interpret uri responder (next filepath)
    Right (Free (GetSource   next)) -> do
      result <- fmap J.virtualFileText
        <$> J.getVirtualFile (J.toNormalizedUri (J.filePathToUri filepath))
      case result of
        Nothing     -> responder (Left [CannotReadFile filepath])
        Just source -> interpret uri responder (next source)
    Right (Free (GetLastSelection next)) -> do
      ref     <- lift $ asks globalSelectionMap
      mapping <- liftIO $ readIORef ref
      let selection = join $ Map.lookup filepath mapping
      interpret uri responder (next selection)
    Right (Free (SetLastSelection selection next)) -> do
      ref <- lift $ asks globalSelectionMap
      liftIO $ modifyIORef' ref (Map.insert filepath (Just selection))
      interpret uri responder next
    Right (Free (GetCachedResult next)) -> do
      result <- readCachedResult filepath
      interpret uri responder (next result)
    Right (Free (SetCacheResult result next)) -> do
      cacheResult filepath result
      interpret uri responder next
    Right (Free (BumpResponseVersion next)) -> do
      n <- bumpVersionM
      interpret uri responder (next n)
    Right (Free (Log text next)) -> do
      logText text
      interpret uri responder next
    Right (Free (SendDiagnostics diagnostics next)) -> do
      -- send diagnostics
      sendDiagnostics filepath diagnostics
      interpret uri responder next
    Left errors -> do
      setMute False -- unmute on error!
      cacheResult filepath (Left errors)
      logText $ Text.pack $ show errors
      responder (Left errors)
