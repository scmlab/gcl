module Server.Monad where

import Control.Concurrent (Chan, newChan, writeChan)
import Control.Monad.Reader
import Data.IORef (IORef, modifyIORef', newIORef, readIORef, writeIORef)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Text (Text)
import qualified Data.Text as Text
import Language.LSP.Server
import Language.LSP.Types hiding (TextDocumentSyncClientCapabilities (..))
import qualified Language.LSP.VFS as VFS

--------------------------------------------------------------------------------

data Env = Env
  { envChan :: Chan Text,
    envDevMode :: Bool,
    -- | We maintain our own Uri-Source mapping
    --   instead of using the built-in LSP VFS
    --   to have better control of update management
    envSourceMap :: IORef (Map FilePath (Text, Maybe (Int, Int))),
    -- | Counter for generating fresh numbers
    envCounter :: IORef Int
  }

initEnv :: Bool -> IO Env
initEnv devMode = Env <$> newChan <*> pure devMode <*> newIORef Map.empty <*> newIORef 0

--------------------------------------------------------------------------------

type ServerM = LspT () (ReaderT Env IO)

runServerM :: Env -> LanguageContextEnv () -> ServerM a -> IO a
runServerM env ctxEnv program = runReaderT (runLspT ctxEnv program) env

--------------------------------------------------------------------------------

-- | Logging instances of Show
logStuff :: Show a => a -> ServerM ()
logStuff x = do
  chan <- lift $ asks envChan
  liftIO $ writeChan chan (Text.pack (show x))

-- | Logging Text
logText :: Text -> ServerM ()
logText text = do
  chan <- lift $ asks envChan
  liftIO $ writeChan chan text

--------------------------------------------------------------------------------

updateSource :: FilePath -> Text -> ServerM ()
updateSource filepath source = do
  ref <- lift $ asks envSourceMap
  liftIO $ modifyIORef' ref (Map.insertWith (\(src, _) (_, sel) -> (src, sel)) filepath (source, Nothing))

readSavedSource :: FilePath -> ServerM (Maybe Text)
readSavedSource filepath = do
  ref <- lift $ asks envSourceMap
  mapping <- liftIO $ readIORef ref
  return $ fst <$> Map.lookup filepath mapping

readLatestSource :: FilePath -> ServerM (Maybe Text)
readLatestSource filepath = fmap VFS.virtualFileText <$> getVirtualFile (toNormalizedUri (filePathToUri filepath))

--------------------------------------------------------------------------------

updateSelection :: FilePath -> (Int, Int) -> ServerM ()
updateSelection filepath selection = do
  ref <- lift $ asks envSourceMap
  liftIO $ modifyIORef' ref (Map.update (\(source, _) -> Just (source, Just selection)) filepath)

readLastSelection :: FilePath -> ServerM (Maybe (Int, Int))
readLastSelection filepath = do
  ref <- lift $ asks envSourceMap
  mapping <- liftIO $ readIORef ref
  return $ snd =<< Map.lookup filepath mapping

--------------------------------------------------------------------------------

bumpCounter :: ServerM Int
bumpCounter = do
  ref <- lift $ asks envCounter
  n <- liftIO $ readIORef ref
  liftIO $ writeIORef ref (succ n)
  return n
