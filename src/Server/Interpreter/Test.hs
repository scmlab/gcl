{-# LANGUAGE OverloadedStrings #-}
module Server.Interpreter.Test (CmdKind(..), TestResult(..), runTest) where

import Control.Monad.Trans.Free
import Control.Monad.Trans.Writer
import Data.Loc.Range
import Data.Text (Text)
import Language.LSP.Types (Diagnostic)
import Server.CustomMethod
import Server.DSL
import Server.Diagnostic (toDiagnostics)
import Render
import qualified Data.Text as Text
import Data.Loc
import Control.Monad.State

data CmdKind
  = CmdEditText Range Text
  | CmdGetFilePath
  | CmdGetSource
  | CmdPutLastSelection Range
  | CmdGetLastSelection
  | CmdBumpResponseVersion
  | CmdLog Text
  | CmdTerminate [ResKind] [Diagnostic]
  deriving (Eq, Show)

type TestM = StateT Text (Writer [CmdKind])

newtype TestResult a = TestResult ((a, Text), [CmdKind])
  deriving (Eq)

instance Show a => Show (TestResult a) where 
  show (TestResult ((value, source), trace)) = "### Result\n\n" <> show value <> "\n\n### Source\n\n" <> Text.unpack source <> "\n\n### Trace\n\n" <> unlines (map show trace)

runTest :: FilePath -> Text -> CmdM a -> TestResult (Maybe a)
runTest filepath source program = TestResult $ runWriter (runStateT (interpret filepath program) source)

interpret :: FilePath -> CmdM a -> TestM (Maybe a)
interpret filepath p = case runCmdM p of
  Right (Pure a) -> return $ Just a 
  Right (Free (EditText range text next)) -> do
    let Range start end = range
    source <- get
    let (before, rest) = Text.splitAt (posCoff start) source 
    let (_, after) = Text.splitAt (posCoff end) rest 
    let newSource = before <> text <> after
    put newSource
    lift $ tell [CmdEditText range text]
    interpret filepath (next newSource)
  Right (Free (GetFilePath next)) -> do
    lift $ tell [CmdGetFilePath]
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
  Right (Free (BumpResponseVersion next)) -> do
    lift $ tell [CmdBumpResponseVersion]
    interpret filepath (next 0)
  Right (Free (Log text next)) -> do
    lift $ tell [CmdLog text]
    interpret filepath next
  Right (Free (Terminate responses diagnostics)) -> do
    -- undefined
    lift $ tell [CmdTerminate responses diagnostics]
    return Nothing
  Left errors -> do
    let responses = [ResDisplay 0 (headerE "Errors" : map renderBlock errors)]
    let diagnostics = errors >>= toDiagnostics
    lift $ tell [CmdTerminate responses diagnostics]
    return Nothing
