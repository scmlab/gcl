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

data CmdKind
  = CmdEditText Range Text
  | CmdGetFilePath
  | CmdGetSource
  | CmdPutLastSelection (Int, Int)
  | CmdGetLastSelection
  | CmdBumpResponseVersion
  | CmdLog Text
  | CmdTerminate [ResKind] [Diagnostic]
  deriving (Eq, Show)

runTest :: FilePath -> CmdM a -> (Maybe a, [CmdKind])
runTest filepath program = runWriter (interpret filepath program)

interpret :: FilePath -> CmdM a -> Writer [CmdKind] (Maybe a)
interpret filepath p = case runCmdM p of
  Right (Pure a) -> return (Just a)
  Right (Free (EditText range text next)) -> do
    tell [CmdEditText range text]
    interpret filepath (next "")
  Right (Free (GetFilePath next)) -> do
    tell [CmdGetFilePath]
    interpret filepath (next filepath)
  Right (Free (GetSource next)) -> do
    tell [CmdGetSource]
    interpret filepath (next "")
  Right (Free (GetLastSelection next)) -> do
    tell [CmdGetLastSelection]
    interpret filepath (next Nothing)
  Right (Free (PutLastSelection selection next)) -> do
    tell [CmdPutLastSelection selection]
    interpret filepath next
  Right (Free (BumpResponseVersion next)) -> do
    tell [CmdBumpResponseVersion]
    interpret filepath (next 0)
  Right (Free (Log text next)) -> do
    tell [CmdLog text]
    interpret filepath next
  Right (Free (Terminate responses diagnostics)) -> do
    -- undefined
    tell [CmdTerminate responses diagnostics]
    return Nothing
  Left errors -> do
    let responses = [ResDisplay 0 (headerE "Errors" : map renderBlock errors)]
    let diagnostics = errors >>= toDiagnostics
    tell [CmdTerminate responses diagnostics]
    return Nothing
