{-# LANGUAGE OverloadedStrings #-}

module Server.Interpreter.Test (CmdKind (..), TestResult (..), runTest) where

import Control.Monad.State
import Control.Monad.Trans.Free
import Control.Monad.Trans.Writer
import Data.Loc
import Data.Loc.Range
import Data.Text (Text)
import qualified Data.Text as Text
import Error (Error)
import Language.LSP.Types (Diagnostic)
import Pretty
import Server.DSL
import Server.Diagnostic (toDiagnostics)

data CmdKind
  = CmdEditText Range Text
  | CmdGetSource
  | CmdPutLastSelection Range
  | CmdGetLastSelection
  | CmdBumpResponseVersion
  | CmdLog Text
  | CmdSendDiagnostics [Diagnostic]
  deriving (Eq, Show)

type TestM = StateT Text (Writer [CmdKind])

newtype TestResult a = TestResult ((Either [Error] a, Text), [CmdKind])
  deriving (Eq, Show)

instance Pretty a => Pretty (TestResult a) where
  pretty (TestResult ((value, source), trace)) =
    "### Result\n\n" <> pretty value <> "\n\n### Source\n\n" <> pretty source <> "\n\n### Trace\n\n" <> pretty (unlines (map show trace))

runTest :: FilePath -> Text -> CmdM (Either [Error] a) -> TestResult a
runTest filepath source program = TestResult $ runWriter (runStateT (interpret filepath program) source)

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
    lift $ tell [CmdEditText range text]
    interpret filepath (next newSource)
  Right (Free (Mute _ next)) -> do
    interpret filepath next
  Right (Free (GetFilePath next)) -> do
    interpret filepath (next filepath)
  Right (Free (GetSource next)) -> do
    lift $ tell [CmdGetSource]
    source <- get
    interpret filepath (next source)
  Right (Free (GetLastSelection next)) -> do
    lift $ tell [CmdGetLastSelection]
    interpret filepath (next Nothing)
  Right (Free (PutLastSelection selection next)) -> do
    lift $ tell [CmdPutLastSelection selection]
    interpret filepath next
  Right (Free (ReadCachedResult next)) -> do
    lift $ tell []
    interpret filepath (next (Right ([], [], [], [])))
  Right (Free (CacheResult _ next)) -> do
    lift $ tell []
    interpret filepath next
  Right (Free (BumpResponseVersion next)) -> do
    lift $ tell [CmdBumpResponseVersion]
    interpret filepath (next 0)
  Right (Free (Log text next)) -> do
    lift $ tell [CmdLog text]
    interpret filepath next
  Right (Free (SendDiagnostics diagnostics next)) -> do
    lift $ tell [CmdSendDiagnostics diagnostics]
    interpret filepath next
  Left errors -> do
    -- let responses = [ResDisplay 0 (headerE "Errors" : map renderBlock errors)]
    let diagnostics = errors >>= toDiagnostics
    lift $ tell [CmdSendDiagnostics diagnostics]
    return $ Left errors
