
module Server where

import           Control.Concurrent             ( forkIO
                                                , readChan
                                                )
import           Control.Monad.Except    hiding ( guard )
import qualified Data.Text.IO                  as Text
import           GHC.IO.IOMode                  ( IOMode(..) )
import           Language.LSP.Server
import qualified Language.LSP.Types            as LSP
                                         hiding ( TextDocumentSyncClientCapabilities(..)
                                                )
import           Network.Simple.TCP             ( HostPreference(Host)
                                                , serve
                                                )
import           Network.Socket                 ( socketToHandle )
import           Server.Handler                 ( handlers )
import Server.Monad (initGlobalEnv, runServerM, logChannel, GlobalState)
import qualified Data.Text as Text

--------------------------------------------------------------------------------

runOnPort :: String -> IO Int
runOnPort port = do
  env <- initGlobalEnv
  _threadId <- forkIO (printLog env)
  serve (Host "127.0.0.1") port $ \(sock, _remoteAddr) -> do
    putStrLn $ "== connection established at " ++ port ++ " =="
    handle <- socketToHandle sock ReadWriteMode
    _      <- runServerWithHandles handle handle (serverDefn env)
    putStrLn "== dev server closed =="
 where
  printLog :: GlobalState -> IO ()
  printLog env = forever $ do
    result <- readChan (logChannel env)
    Text.putStrLn result

-- entry point of the LSP server
runOnStdio :: Maybe FilePath -> IO Int
runOnStdio maybeLogFile = do
  env <- initGlobalEnv
  case maybeLogFile of
    Nothing -> return ()
    Just logFile -> do
      writeFile logFile "=== Log file Start ===\n"
      writeFile logFile logFile
      _threadId <- forkIO (writeLog env logFile)
      return ()
  runServer (serverDefn env)
 where
  writeLog :: GlobalState -> FilePath -> IO ()
  writeLog env logFile = forever $ do
    result <- readChan (logChannel env)
    appendFile logFile (Text.unpack result)

serverDefn :: GlobalState -> ServerDefinition ()
serverDefn env = ServerDefinition
  { defaultConfig         = ()
  , onConfigurationChange = const $ pure $ Right ()
  , doInitialize          = \ctxEnv _req -> pure $ Right ctxEnv
  , staticHandlers        = handlers
  , interpretHandler      = \ctxEnv -> Iso (runServerM env ctxEnv) liftIO
  , options               = lspOptions
  }

lspOptions :: Options
lspOptions = defaultOptions { textDocumentSync            = Just syncOptions
                            , completionTriggerCharacters = Just ['\\']
                            }

-- these `TextDocumentSyncOptions` are essential for receiving notifications from the client
syncOptions :: LSP.TextDocumentSyncOptions
syncOptions = LSP.TextDocumentSyncOptions
  { LSP._openClose         = Just True -- receive open and close notifications from the client
  , LSP._change            = Just LSP.TdSyncIncremental -- receive change notifications from the client
  , LSP._willSave          = Just False -- receive willSave notifications from the client
  , LSP._willSaveWaitUntil = Just False -- receive willSave notifications from the client
  , LSP._save              = Just $ LSP.InR saveOptions
  }

-- includes the document content on save, so that we don't have to read it from the disk
saveOptions :: LSP.SaveOptions
saveOptions = LSP.SaveOptions (Just True)
