{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}

module Test.Server.Interpreter
  ( Trace(..)
  , TestResult(..)
  , runTest
  , serializeTestResult
  , serializeTestResultValueOnly
  ) where

import           Control.Monad.RWS       hiding ( state )
import           Control.Monad.Trans.Free
import qualified Data.ByteString.Lazy          as BSL
import           Data.Loc
import           Data.Loc.Range
import           Data.Text                      ( Text )
import qualified Data.Text                     as Text
import qualified Data.Text.Encoding            as Text
import           Language.LSP.Types             ( Diagnostic )
import           Pretty
import           Server.Pipeline
import Error (Error)

--------------------------------------------------------------------------------

data TestResult a = TestResult
  { testResultValue  :: Either [Error] a
  , testResultSource :: Text
  , testResultState  :: PipelineState
  , testResultTrace  :: [Trace]
  }
  deriving (Eq, Show)

instance Pretty a => Pretty (TestResult a) where
  pretty (TestResult value source _state trace) =
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

-- for logging side-effects 
data Trace
  = TraceEditText Range Text
  | TraceGetSource
  | TraceLog Text
  | TraceSendDiagnostics [Diagnostic]
  | TraceSolve Text
  deriving (Eq, Show)

type TestM a = RWS FilePath [Trace] (Text, PipelineState) a

runTest :: FilePath -> Text -> PipelineM (Either [Error] a) -> TestResult a
runTest filepath source program =
  let (result, (text, finalState), trace) =
        runRWS (interpret program) filepath (source, initState filepath)
  in  TestResult result text finalState trace

-- Interprets the Server DSL and logs side effects as [Trace] 
interpret :: PipelineM (Either [Error] a) -> TestM (Either [Error] a)
interpret p = do
  filepath <- ask
  case runPipelineM filepath (initState filepath) p of
    Right (Pure value, newState, _) -> do
      -- store the new state 
      modify' $ \(source, _) -> (source, newState)
      return value
    Right (Free command, newState, _) -> do
      -- store the new state 
      modify' $ \(source, _) -> (source, newState)
      go command
    Left errors -> do
      -- got errors from computation
      PipelineState _ cachedStage _ selections counter <- gets snd
      let newState =
            PipelineState errors -- store it for later inspection 
                            cachedStage False -- unmute on error!
                                              selections counter
      modify' $ \(source, _) -> (source, newState)
      return $ Left errors

-- Interprets the Server DSL and logs side effects as [Trace] 
go :: Instruction (PipelineM (Either [Error] a)) -> TestM (Either [Error] a)
go = \case
  EditText range text next -> do
    let Range start end = range
    source <- gets fst
    let (before, rest) = Text.splitAt (posCoff start) source
    let (_, after)     = Text.splitAt (posCoff end - posCoff start) rest
    let newSource      = before <> text <> after
    modify' $ \(_, oldState) -> (newSource, oldState)
    tell [TraceEditText range text]
    interpret (next newSource)
  GetSource next -> do
    tell [TraceGetSource]
    source <- gets fst
    interpret (next source)
  Log text next -> do
    tell [TraceLog text]
    interpret next
  SendDiagnostics diagnostics next -> do
    tell [TraceSendDiagnostics diagnostics]
    interpret next
