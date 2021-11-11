{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}

module Server.Monad
  ( ServerM
  , GlobalEnv(globalChan)
  , initGlobalEnv
  , runServerM
  , customRequestResponder
  , notificationResponder
  , interpret
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
                                                , Stage(..)
                                                , runCmdM
                                                )
import qualified Server.DSL                    as DSL
import           Server.Handler.Diagnostic      ( collect )
import qualified Server.SrcLoc                 as SrcLoc
import Data.Maybe (isJust)

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
  , globalCurrentStage :: IORef (Map FilePath ([Error], Stage))
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

convertErrors :: FilePath -> [Error] -> ServerM [ResKind]
convertErrors filepath errors = do
  version <- bumpVersionM
  let responses =
        [ResDisplay version (map renderSection errors), ResUpdateSpecs []]
  let diagnostics = errors >>= collect
  -- send diagnostics
  sendDiagnostics filepath diagnostics
  return responses

customRequestResponder
  :: FilePath
  -> (Response -> ServerM ())
  -> ([Error], Maybe [ResKind])
  -> ServerM ()
customRequestResponder filepath responder (_errors, _result) = do
  -- TEMP: ignore all results from custom handlers
  -- (oldErrors, result)       <- getState filepath
  -- responsesFromError <- convertErrors filepath oldErrors
  -- let responses = case result of
  --       Nothing -> responsesFromError
  --       Just xs -> responsesFromError <> xs
  responder (Res filepath [])

  -- (oldErrors, _)       <- getState filepath
  -- responsesFromError <- convertErrors filepath errors
  -- let responses = case result of
  --       Nothing -> responsesFromError
  --       Just xs -> responsesFromError <> xs
  -- responder (Res filepath responses)

notificationResponder :: J.Uri -> ([Error], Maybe [ResKind]) -> ServerM ()
notificationResponder uri (errors, result) = case J.uriToFilePath uri of
  Nothing       -> pure ()
  Just filepath -> do
    logText $ " ### State: " <> toText (length errors, isJust result)
    responsesFromError <- convertErrors filepath errors
    let responses = case result of
          Nothing -> responsesFromError
          Just xs -> responsesFromError <> xs
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

setState :: FilePath -> ([Error], Stage) -> ServerM ()
setState filepath state = do
  ref <- lift $ asks globalCurrentStage
  liftIO $ modifyIORef' ref (Map.insert filepath state)

getState :: FilePath -> ServerM ([Error], Stage)
getState filepath = do
  ref     <- lift $ asks globalCurrentStage
  mapping <- liftIO $ readIORef ref
  case Map.lookup filepath mapping of
    Nothing    -> return ([], Uninitialized filepath)
    Just state -> return state

--------------------------------------------------------------------------------

interpret
  :: Show a
  => J.Uri
  -> (([Error], Maybe a) -> ServerM ())
  -> CmdM a
  -> ServerM ()
interpret uri responder p = case J.uriToFilePath uri of
  Nothing       -> pure ()
  Just filepath -> case runCmdM p of
    Right result -> go filepath result
    Left  errors -> do
      setMute False -- unmute on error!
      -- get the latest cached result 
      state <- getState filepath
      setState filepath state
      responder (errors, Nothing)

 where
  go filepath = \case
    Pure responses -> do
      -- send responses
      (errors, _) <- getState filepath
      responder (errors, Just responses)
      -- sendResponses filepath responder responses
    Free (EditText range text next) -> do
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
    Free (SetMute b next) -> do
      setMute b
      interpret uri responder next
    Free (GetMute next) -> do
      m <- getMute
      interpret uri responder (next m)
    Free (GetFilePath next) -> interpret uri responder (next filepath)
    Free (GetSource   next) -> do
      result <- fmap J.virtualFileText
        <$> J.getVirtualFile (J.toNormalizedUri (J.filePathToUri filepath))
      case result of
        Nothing     -> responder ([CannotReadFile filepath], Nothing)
        Just source -> interpret uri responder (next source)
    Free (GetLastSelection next) -> do
      ref     <- lift $ asks globalSelectionMap
      mapping <- liftIO $ readIORef ref
      let selection = join $ Map.lookup filepath mapping
      interpret uri responder (next selection)
    Free (SetLastSelection selection next) -> do
      ref <- lift $ asks globalSelectionMap
      liftIO $ modifyIORef' ref (Map.insert filepath (Just selection))
      interpret uri responder next
    Free (GetCurrentState next) -> do
      state <- getState filepath
      interpret uri responder $ do
        next state
    Free (SetCurrentState (newError, newStage) next) -> do
      (old, oldStage) <- getState filepath
      if null old
        -- overwrite with a new stage 
        then setState filepath (newError, newStage)
        -- do nothing 
        else setState filepath (old, oldStage)
      interpret uri responder next
    Free (BumpResponseVersion next) -> do
      n <- bumpVersionM
      interpret uri responder (next n)
    Free (Log text next) -> do
      logText text
      interpret uri responder next
    Free (SendDiagnostics diagnostics next) -> do
      -- send diagnostics
      sendDiagnostics filepath diagnostics
      interpret uri responder next
