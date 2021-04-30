{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
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
import Language.LSP.Diagnostics
import Data.Aeson (ToJSON)
import GHC.Generics (Generic)
import qualified Syntax.Abstract as A
import Data.Loc
import Error
import Syntax.Predicate (PO, Spec, Origin)
import GCL.WP
import qualified Data.Aeson as JSON

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


--------------------------------------------------------------------------------

sendDiagnostics :: FilePath -> [Diagnostic] -> ServerM ()
sendDiagnostics filepath diagnostics = do 
  -- send diagnostics
  version <- bumpCounter
  publishDiagnostics 100 (toNormalizedUri (filePathToUri filepath)) (Just version) (partitionBySource diagnostics)

type Responder = Response -> ServerM ()

sendResponses :: FilePath -> Maybe Responder -> [ResKind] -> ServerM ()
sendResponses filepath responder responses = do 
    -- send responses
    case responder of
      Nothing -> sendNotification (SCustomMethod "guacamole") $ JSON.toJSON $ Res filepath responses
      Just f -> f $ Res filepath responses

--------------------------------------------------------------------------------

type ID = LspId ('CustomMethod :: Method 'FromClient 'Request)

-- | Response
data ResKind
  = ResOK ID [PO] [Spec] [A.Expr] [StructWarning]
  | ResInspect [PO]
  | ResError [(Site, Error)]
  | ResUpdateSpecPositions [Loc]
  | ResResolve Int -- resolves some Spec
  | ResSubstitute Int A.Expr
  | ResConsoleLog Text
  deriving (Generic)

instance ToJSON ResKind

instance Show ResKind where
  show (ResOK i pos specs props warnings) =
    "OK " <> show i <> " "
      <> show (length pos)
      <> " pos, "
      <> show (length specs)
      <> " specs, "
      <> show (length props)
      <> " props, "
      <> show (length warnings)
      <> " warnings"
  show (ResInspect pos) = "Inspect " <> show (length pos) <> " POs"
  show (ResError errors) = "Error " <> show (length errors) <> " errors"
  show (ResUpdateSpecPositions locs) = "UpdateSpecPositions " <> show (length locs) <> " locs"
  show (ResResolve i) = "Resolve " <> show i
  show (ResSubstitute i _) = "Substitute " <> show i
  show (ResConsoleLog x) = "ConsoleLog " <> show x

data Response
  = Res FilePath [ResKind]
  | CannotDecodeRequest String
  | NotLoaded
  deriving (Generic)

instance ToJSON Response

instance Show Response where
  show (Res _path kinds) = show kinds
  show (CannotDecodeRequest s) = "CannotDecodeRequest " <> s
  show NotLoaded = "NotLoaded"

--------------------------------------------------------------------------------

-- | Instances of ToJSON
instance ToJSON Origin

instance ToJSON PO

instance ToJSON Spec
