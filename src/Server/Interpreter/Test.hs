{-# LANGUAGE OverloadedStrings #-}
module Server.Interpreter.Test (CmdKind(..), runTest) where

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

runTest :: FilePath -> Text -> CmdM a -> ((Maybe a, Text), [CmdKind])
runTest filepath source program = runWriter (runStateT (interpret filepath source program) source)

interpret :: FilePath -> Text -> CmdM a -> TestM (Maybe a)
interpret filepath source p = case runCmdM p of
  Right (Pure a) -> return $ Just a 
  Right (Free (EditText range text next)) -> do
    let Range start end = range
    let (before, rest) = Text.splitAt (posCoff start) source 
    let (_, after) = Text.splitAt (posCoff end) rest 
    let newSource = before <> text <> after
    let msg = "original: {" <> source <> "} before: {" <> before <> "} text: {" <> text <> "} after: {" <> after <> "}"
    -- let msg = "total: " <> Text.pack (show (Text.length text)) <> " " <> Text.pack (show (posCoff start, posCoff end)) <> " " <> text
    lift $ tell [CmdLog msg, CmdEditText range text]
    interpret filepath newSource (next newSource)
  Right (Free (GetFilePath next)) -> do
    lift $ tell [CmdGetFilePath]
    interpret filepath source (next filepath)
  Right (Free (GetSource next)) -> do
    lift $ tell [CmdGetSource]
    interpret filepath source (next source)
  Right (Free (GetLastSelection next)) -> do
    lift $ tell [CmdGetLastSelection]
    interpret filepath source (next Nothing)
  Right (Free (PutLastSelection selection next)) -> do
    lift $ tell [CmdPutLastSelection selection]
    interpret filepath source next
  Right (Free (BumpResponseVersion next)) -> do
    lift $ tell [CmdBumpResponseVersion]
    interpret filepath source (next 0)
  Right (Free (Log text next)) -> do
    lift $ tell [CmdLog text]
    interpret filepath source next
  Right (Free (Terminate responses diagnostics)) -> do
    -- undefined
    lift $ tell [CmdTerminate responses diagnostics]
    return Nothing
  Left errors -> do
    let responses = [ResDisplay 0 (headerE "Errors" : map renderBlock errors)]
    let diagnostics = errors >>= toDiagnostics
    lift $ tell [CmdTerminate responses diagnostics]
    return Nothing
