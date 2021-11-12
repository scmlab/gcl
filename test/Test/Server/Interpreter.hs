{-# LANGUAGE OverloadedStrings #-}

module Test.Server.Interpreter
  ( Trace(..)
  , TestResult(..)
  , runTest
  , serializeTestResult
  , serializeTestResultValueOnly
  ) where

import           Control.Monad.RWS
import           Control.Monad.Trans.Free
import qualified Data.ByteString.Lazy          as BSL
import           Data.Loc
import           Data.Loc.Range
import           Data.Text                      ( Text )
import qualified Data.Text                     as Text
import qualified Data.Text.Encoding            as Text
import           Error                          ( Error(CannotReadFile) )
import           Language.LSP.Types             ( Diagnostic )
import           Pretty
import           Server.DSL
import           Server.Handler.Diagnostic      ( collect )

--------------------------------------------------------------------------------

data TestResult a = TestResult
  { testResultValue  :: Either [Error] a
  , testResultSource :: Text
  , testResultTrace  :: [Trace]
  }
  deriving (Eq, Show)

instance Pretty a => Pretty (TestResult a) where
  pretty (TestResult value source trace) =
    "### Result\n\n"
      <> pretty value
      <> "\n\n### Source\n\n"
      <> pretty source
      <> "\n\n### Trace\n\n"
      <> pretty (unlines (map show trace))

-- | Serialize TestResult for Golden tests
serializeTestResult :: Pretty a => TestResult a -> BSL.ByteString
serializeTestResult = BSL.fromStrict . Text.encodeUtf8 . toText

serializeTestResultValueOnly :: Pretty a => TestResult a -> BSL.ByteString
serializeTestResultValueOnly =
  BSL.fromStrict . Text.encodeUtf8 . toText . testResultValue

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

type TestM a = RWS () [Trace] Text a

runTest :: FilePath -> Text -> CmdM (Either [Error] a) -> TestResult a
runTest filepath source program =
  let (result, text, trace) = runRWS (interpret filepath program) () source
  in  TestResult result text trace

-- Interprets the Server DSL and logs side effects as [Trace] 
interpret :: FilePath -> CmdM (Either [Error] a) -> TestM (Either [Error] a)
interpret filepath p = case runCmdM p of
  Right (Pure result                    ) -> return result
  Right (Free (EditText range text next)) -> do
    let Range start end = range
    source <- get
    let (before, rest) = Text.splitAt (posCoff start) source
    let (_, after)     = Text.splitAt (posCoff end - posCoff start) rest
    let newSource      = before <> text <> after
    put newSource
    tell [TraceEditText range text]
    interpret filepath (next newSource)
  Right (Free (SetMute _ next)) -> do
    interpret filepath next
  Right (Free (GetMute next)) -> do
    interpret filepath (next False)
  Right (Free (GetFilePath next)) -> do
    interpret filepath (next filepath)
  Right (Free (GetSource next)) -> do
    tell [TraceGetSource]
    source <- get
    interpret filepath (next source)
  Right (Free (GetLastSelection next)) -> do
    tell [TraceGetLastSelection]
    interpret filepath (next Nothing)
  Right (Free (SetLastSelection selection next)) -> do
    tell [TracePutLastSelection selection]
    interpret filepath next
  Right (Free (GetCurrentState _next)) -> do
    tell []
    interpret filepath $ return $ Left [CannotReadFile filepath]
  Right (Free (SetCurrentState _ next)) -> do
    tell []
    interpret filepath next
  Right (Free (BumpResponseVersion next)) -> do
    tell [TraceBumpResponseVersion]
    interpret filepath (next 0)
  Right (Free (Log text next)) -> do
    tell [TraceLog text]
    interpret filepath next
  Right (Free (SendDiagnostics diagnostics next)) -> do
    tell [TraceSendDiagnostics diagnostics]
    interpret filepath next
  Left errors -> do
    -- let responses = [ResDisplay 0 (headerE "Errors" : map renderSection errors)]
    let diagnostics = errors >>= collect
    tell [TraceSendDiagnostics diagnostics]
    return $ Left errors
