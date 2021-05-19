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
  | CmdAskFilePath
  | CmdGetSource
  | CmdPutLastSelection (Int, Int)
  | CmdGetLastSelection
  | CmdBumpResponseVersion
  | CmdLog Text
  | CmdTerminate [ResKind] [Diagnostic]
  deriving (Eq, Show)

runTest :: CmdM a -> (Maybe a, [CmdKind])
runTest program = runWriter (interpret program)

interpret :: CmdM a -> Writer [CmdKind] (Maybe a)
interpret p = case runCmdM p of
  Right (Pure a) -> return (Just a)
  Right (Free (EditText range text next)) -> do
    tell [CmdEditText range text]
    interpret (next "")
  Right (Free (GetFilePath next)) -> do
    tell [CmdAskFilePath]
    interpret (next "")
  Right (Free (GetSource next)) -> do
    tell [CmdGetSource]
    interpret (next "")
  Right (Free (GetLastSelection next)) -> do
    tell [CmdGetLastSelection]
    interpret (next Nothing)
  Right (Free (PutLastSelection selection next)) -> do
    tell [CmdPutLastSelection selection]
    interpret next
  Right (Free (BumpResponseVersion next)) -> do
    tell [CmdBumpResponseVersion]
    interpret (next 0)
  Right (Free (Log text next)) -> do
    tell [CmdLog text]
    interpret next
  Right (Free (Terminate responses diagnostics)) -> do
    -- undefined
    tell [CmdTerminate responses diagnostics]
    return Nothing
  Left errors -> do
    let responses = [ResDisplay 0 (headerE "Errors" : map renderBlock errors)]
    let diagnostics = errors >>= toDiagnostics
    tell [CmdTerminate responses diagnostics]
    return Nothing
