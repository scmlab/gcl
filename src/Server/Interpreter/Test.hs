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

runTest :: FilePath -> Text -> CmdM a -> (Maybe a, [CmdKind])
runTest filepath source program = runWriter (interpret filepath source program)

interpret :: FilePath -> Text -> CmdM a -> Writer [CmdKind] (Maybe a)
interpret filepath source p = case runCmdM p of
  Right (Pure a) -> return (Just a)
  Right (Free (EditText range text next)) -> do
    let Range start end = range
    let (before, rest) = Text.splitAt (posCoff start) source 
    let (_, after) = Text.splitAt (posCoff end) rest 
    let newSource = before <> text <> after
    tell [CmdEditText range text]
    interpret filepath newSource (next newSource)
  Right (Free (GetFilePath next)) -> do
    tell [CmdGetFilePath]
    interpret filepath source (next filepath)
  Right (Free (GetSource next)) -> do
    tell [CmdGetSource]
    interpret filepath source (next source)
  Right (Free (GetLastSelection next)) -> do
    tell [CmdGetLastSelection]
    interpret filepath source (next Nothing)
  Right (Free (PutLastSelection selection next)) -> do
    tell [CmdPutLastSelection selection]
    interpret filepath source next
  Right (Free (BumpResponseVersion next)) -> do
    tell [CmdBumpResponseVersion]
    interpret filepath source (next 0)
  Right (Free (Log text next)) -> do
    tell [CmdLog text]
    interpret filepath source next
  Right (Free (Terminate responses diagnostics)) -> do
    -- undefined
    tell [CmdTerminate responses diagnostics]
    return Nothing
  Left errors -> do
    let responses = [ResDisplay 0 (headerE "Errors" : map renderBlock errors)]
    let diagnostics = errors >>= toDiagnostics
    tell [CmdTerminate responses diagnostics]
    return Nothing
