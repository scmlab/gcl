{-# LANGUAGE OverloadedStrings #-}

module Server.Interpreter.Test (Trace (..), TestResult (..), runTest, serializeTestResult) where

import Control.Monad.State
import Control.Monad.Trans.Free
import Control.Monad.Trans.Writer
import qualified Data.ByteString.Lazy as BSL
import Data.Loc
import Data.Loc.Range
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
import Error (Error)
import Language.LSP.Types (Diagnostic)
import Pretty
import Server.DSL
import Server.Stab (collect)

--------------------------------------------------------------------------------

data TestResult a = TestResult
  { testResultValue :: Either [Error] a,
    testResultSource :: Text,
    testResultTrace :: [Trace]
  }
  deriving (Eq, Show)

instance Pretty a => Pretty (TestResult a) where
  pretty (TestResult value source trace) =
    "### Result\n\n" <> pretty value <> "\n\n### Source\n\n" <> pretty source <> "\n\n### Trace\n\n" <> pretty (unlines (map show trace))

-- | Serialize TestResult for Golden tests
serializeTestResult :: Pretty a => TestResult a -> BSL.ByteString
serializeTestResult = BSL.fromStrict . Text.encodeUtf8 . toText 

--------------------------------------------------------------------------------

data Trace
  = TraceEditText Range Text
  | TraceGetSource
  | TracePutLastSelection Range
  | TraceGetLastSelection
  | TraceBumpResponseVersion
  | TraceLog Text
  | TraceSendDiagnostics [Diagnostic]
  deriving (Eq, Show)

type TestM = StateT Text (Writer [Trace])

runTest :: FilePath -> Text -> CmdM (Either [Error] a) -> TestResult a
runTest filepath source program = uncurry (uncurry TestResult) $ runWriter (runStateT (interpret filepath program) source)

interpret :: FilePath -> CmdM (Either [Error] a) -> TestM (Either [Error] a)
interpret filepath p = case runCmdM p of
  Right (Pure result) -> return result
  Right (Free (EditText range text next)) -> do
    let Range start end = range
    source <- get
    let (before, rest) = Text.splitAt (posCoff start) source
    let (_, after) = Text.splitAt (posCoff end - posCoff start) rest
    let newSource = before <> text <> after
    put newSource
    lift $ tell [TraceEditText range text]
    interpret filepath (next newSource)
  Right (Free (Mute _ next)) -> do
    interpret filepath next
  Right (Free (GetFilePath next)) -> do
    interpret filepath (next filepath)
  Right (Free (GetSource next)) -> do
    lift $ tell [TraceGetSource]
    source <- get
    interpret filepath (next source)
  Right (Free (GetLastSelection next)) -> do
    lift $ tell [TraceGetLastSelection]
    interpret filepath (next Nothing)
  Right (Free (PutLastSelection selection next)) -> do
    lift $ tell [TracePutLastSelection selection]
    interpret filepath next
  Right (Free (ReadCachedResult next)) -> do
    lift $ tell []
    interpret filepath (next Nothing)
  Right (Free (CacheResult _ next)) -> do
    lift $ tell []
    interpret filepath next
  Right (Free (BumpResponseVersion next)) -> do
    lift $ tell [TraceBumpResponseVersion]
    interpret filepath (next 0)
  Right (Free (Log text next)) -> do
    lift $ tell [TraceLog text]
    interpret filepath next
  Right (Free (SendDiagnostics diagnostics next)) -> do
    lift $ tell [TraceSendDiagnostics diagnostics]
    interpret filepath next
  Left errors -> do
    -- let responses = [ResDisplay 0 (headerE "Errors" : map renderSection errors)]
    let diagnostics = errors >>= collect
    lift $ tell [TraceSendDiagnostics diagnostics]
    return $ Left errors
