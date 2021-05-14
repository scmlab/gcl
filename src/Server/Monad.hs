{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE OverloadedStrings #-}

module Server.Monad where

import Control.Concurrent (Chan, newChan, writeChan)
import Control.Monad.Reader
import Data.Aeson (FromJSON, ToJSON)
import qualified Data.Aeson as JSON
import Data.IORef (IORef, newIORef, readIORef, writeIORef)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Text (Text)
import qualified Data.Text as Text
import GHC.Generics (Generic)
import Language.LSP.Diagnostics
import Language.LSP.Server
import Language.LSP.Types hiding (TextDocumentSyncClientCapabilities (..))
import Render
import qualified Syntax.Abstract as A
import Syntax.Predicate (Origin, PO, Spec)
import Data.Loc (Loc)

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
logText s = do
  chan <- lift $ asks envChan
  liftIO $ writeChan chan s

--------------------------------------------------------------------------------

sendDiagnostics :: FilePath -> [Diagnostic] -> ServerM ()
sendDiagnostics filepath diagnostics = do
  -- send diagnostics
  ref <- lift $ asks envCounter
  version <- liftIO $ readIORef ref
  liftIO $ writeIORef ref (succ version)
  publishDiagnostics 100 (toNormalizedUri (filePathToUri filepath)) (Just version) (partitionBySource diagnostics)

type Responder = Response -> ServerM ()

sendResponses :: FilePath -> Maybe Responder -> [ResKind] -> ServerM ()
sendResponses filepath responder responses = do
  -- send responses
  case responder of
    Nothing -> sendNotification (SCustomMethod "guacamole") $ JSON.toJSON $ Res filepath responses
    Just f -> f $ Res filepath responses

--------------------------------------------------------------------------------

-- | Response
data ResKind
  = ResDisplay Int [Block]
  | ResUpdateSpecs [(Int, Text, Text, Loc)]
  | ResSubstitute Int A.Expr
  | ResConsoleLog Text
  deriving (Eq, Generic)

instance ToJSON ResKind

instance Show ResKind where
  show (ResDisplay _ _) = "Display"
  show (ResUpdateSpecs _) = "UpdateSpecs"
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

-- | Request
data ReqKind
  = ReqInspect Int Int
  | ReqRefine Int Int
  | ReqSubstitute Int A.Expr A.Subst
  | ReqExportProofObligations
  | ReqDebug
  deriving (Generic)

instance FromJSON ReqKind

instance Show ReqKind where
  show (ReqInspect x y) = "Inspect " <> show x <> " " <> show y
  show (ReqRefine i x) = "Refine #" <> show i <> " " <> show x
  show (ReqSubstitute i x y) = "Substitute #" <> show i <> " " <> show x <> " => " <> show y
  show ReqExportProofObligations = "ExportProofObligations"
  show ReqDebug = "Debug"

data Request = Req FilePath ReqKind
  deriving (Generic)

instance FromJSON Request

instance Show Request where
  show (Req _path kind) = show kind

--------------------------------------------------------------------------------

-- | Instances of ToJSON
instance ToJSON Origin

instance ToJSON PO

instance ToJSON Spec
