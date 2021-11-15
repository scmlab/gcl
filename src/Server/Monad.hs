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
import           Data.Map                       ( Map )
import qualified Data.Map                      as Map
import qualified Data.Maybe                    as Maybe
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
                                                , CmdState(..)
                                                , initState
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
    -- globalSelectionMap :: IORef (Map FilePath (Maybe Range))
  -- ,
    -- Counter for generating fresh numbers
    globalCounter      :: IORef Int
  , globalCurrentStage :: IORef (Map FilePath CmdState)
  }

-- | Constructs an initial global state
initGlobalEnv :: IO GlobalEnv
initGlobalEnv =
  GlobalEnv
    <$> newChan
    -- <*> newIORef Map.empty
    <*> newIORef 0
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

-- send diagnostics
sendDiagnostics :: FilePath -> [J.Diagnostic] -> ServerM ()
sendDiagnostics filepath diagnostics = do
  version <- bumpVersionM
  -- only send diagnostics when it's not empty
  -- otherwise the existing diagnostics would be erased 
  unless (null diagnostics) $ do
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

  sendDiagnostics filepath diagnostics

  return responses

customRequestResponder
  :: FilePath
  -> (Response -> ServerM ())
  -> ([Error], Maybe [ResKind])
  -> ServerM ()
customRequestResponder filepath responder (errors, result) = if null errors
  then do
    let responses = Maybe.fromMaybe [] result
    responder (Res filepath responses)
  else do
    responsesFromError <- convertErrors filepath errors
    responder (Res filepath responsesFromError)

notificationResponder :: J.Uri -> ([Error], Maybe [ResKind]) -> ServerM ()
notificationResponder uri (errors, result) = case J.uriToFilePath uri of
  Nothing       -> pure ()
  Just filepath -> do
    responsesFromError <- convertErrors filepath errors
    let responses = case result of
          Nothing -> responsesFromError
          Just xs -> responsesFromError <> xs

    logText
      $  " <--- Respond with "
      <> toText (length result)
      <> " responses and "
      <> toText (length errors)
      <> " errors"
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

setState :: FilePath -> CmdState -> ServerM ()
setState filepath state = do
  ref <- lift $ asks globalCurrentStage
  liftIO $ modifyIORef' ref (Map.insert filepath state)

getState :: FilePath -> ServerM CmdState
getState filepath = do
  ref     <- lift $ asks globalCurrentStage
  mapping <- liftIO $ readIORef ref
  case Map.lookup filepath mapping of
    Nothing    -> return $ initState filepath
    Just state -> return state

--------------------------------------------------------------------------------


interpret
  :: Show a
  => J.Uri
  -> (([Error], Maybe a) -> ServerM ())
  -> CmdM a
  -> ServerM ()
interpret uri responcder p = case J.uriToFilePath uri of
  Nothing       -> pure ()
  Just filepath -> do
    state <- getState filepath
    interpret2 state filepath responcder p

interpret2
  :: Show a
  => CmdState
  -> FilePath
  -> (([Error], Maybe a) -> ServerM ())
  -> CmdM a
  -> ServerM ()
interpret2 state filepath responder p = case runCmdM filepath state p of
  Right (Pure value, newState, ()) -> do
    -- store the new state 
    logText "[SUCCESS]"
    setState filepath newState
    responder (cmdErrors newState, Just value)
  Right (Free command, newState, ()) -> go filepath newState responder command
  Left  errors                       -> do
    -- got errors from computation
    CmdState _ cachedStage _ selections counter <- getState filepath
    let newState =
          CmdState errors -- store it for later inspection 
                          cachedStage False -- unmute on error!
                                            selections counter
    logText "[ERROR]"
    setState filepath newState
    responder (errors, Nothing)

go
  :: Show a
  => FilePath
  -> CmdState
  -> (([Error], Maybe a) -> ServerM ())
  -> Cmd (CmdM a)
  -> ServerM ()
go filepath state responder = \case
  EditText range text next -> do
    logText $ " ### EditText " <> toText range <> " " <> text
    -- apply edit
    let removeSpec = J.TextEdit (SrcLoc.toLSPRange range) text

    let identifier = J.VersionedTextDocumentIdentifier
          (J.filePathToUri filepath)
          (Just 0)
    let textDocumentEdit =
          J.TextDocumentEdit identifier (J.List [J.InL removeSpec])
    let change = J.InL textDocumentEdit
    let workspaceEdit =
          J.WorkspaceEdit Nothing (Just (J.List [change])) Nothing
    let applyWorkspaceEditParams =
          J.ApplyWorkspaceEditParams (Just "Resolve Spec") workspaceEdit
    let callback _ = interpret2 state filepath responder $ do
          DSL.getSource >>= next

    void $ J.sendRequest J.SWorkspaceApplyEdit applyWorkspaceEditParams callback
  GetSource next -> do
    result <- fmap J.virtualFileText
      <$> J.getVirtualFile (J.toNormalizedUri (J.filePathToUri filepath))
    case result of
      Nothing     -> responder ([CannotReadFile filepath], Nothing)
      Just source -> interpret2 state filepath responder (next source)
  Log text next -> do
    logText text
    interpret2 state filepath responder next
  SendDiagnostics diagnostics next -> do
    -- send diagnostics
    sendDiagnostics filepath diagnostics
    interpret2 state filepath responder next
