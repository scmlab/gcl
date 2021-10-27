module Server where

import Control.Concurrent (forkIO, readChan)
import Control.Monad.Except hiding (guard)
import qualified Data.Text.IO as Text
import GHC.IO.IOMode (IOMode (ReadWriteMode))
import Server.Handler (handlers)
import Server.Monad
import Language.LSP.Server
import qualified Language.LSP.Types as LSP hiding (TextDocumentSyncClientCapabilities (..))
import Network.Simple.TCP (HostPreference (Host), serve)
import Network.Socket (socketToHandle)

--------------------------------------------------------------------------------

-- entry point of the LSP server
run :: Bool -> IO Int
run devMode = do
  env <- initGlobalEnv  
  if devMode
    then do
      let port = "3000"
      _ <- forkIO (printLog env)
      serve (Host "127.0.0.1") port $ \(sock, _remoteAddr) -> do
        putStrLn $ "== connection established at " ++ port ++ " =="
        handle <- socketToHandle sock ReadWriteMode
        _ <- runServerWithHandles handle handle (serverDefn env)
        putStrLn "== dev server closed =="
    else do
      runServer (serverDefn env)
  where
    printLog :: GlobalEnv -> IO ()
    printLog env = forever $ do
      result <- readChan (globalChan env)
      when devMode $ do
        Text.putStrLn result

    serverDefn :: GlobalEnv -> ServerDefinition ()
    serverDefn env =
      ServerDefinition
        { defaultConfig = (),
          onConfigurationChange = const $ pure $ Right (),
          doInitialize = \ctxEnv _req -> pure $ Right ctxEnv,
          staticHandlers = handlers,
          interpretHandler = \ctxEnv -> Iso (runServerM env ctxEnv) liftIO,
          options = lspOptions
        }

    lspOptions :: Options
    lspOptions =
      defaultOptions
        { textDocumentSync = Just syncOptions
        , completionTriggerCharacters = Just ['\\']
        }

    -- these `TextDocumentSyncOptions` are essential for receiving notifications from the client
    syncOptions :: LSP.TextDocumentSyncOptions
    syncOptions =
      LSP.TextDocumentSyncOptions
        { LSP._openClose = Just True, -- receive open and close notifications from the client
          LSP._change = Just LSP.TdSyncIncremental, -- receive change notifications from the client
          LSP._willSave = Just False, -- receive willSave notifications from the client
          LSP._willSaveWaitUntil = Just False, -- receive willSave notifications from the client
          LSP._save = Just $ LSP.InR saveOptions
        }

    -- includes the document content on save, so that we don't have to read it from the disk
    saveOptions :: LSP.SaveOptions
    saveOptions = LSP.SaveOptions (Just True)
