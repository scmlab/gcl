{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}

module Server.Handler2.Utils where

import qualified Language.LSP.VFS as LSP
import Server.Monad (ServerM, LoadedProgram, GlobalEnv (..))
import qualified Server.Monad
import Data.Text (Text)
import qualified Language.LSP.Types as LSP
import qualified Language.LSP.Server as LSP
import Data.Loc.Range (Range, rangeFile)
import qualified Server.SrcLoc as SrcLoc
import qualified Data.Map as Map
import Control.Monad.Reader.Class (asks)
import Data.IORef (readIORef, modifyIORef')
import Control.Monad.Cont (liftIO)
import Control.Monad.Trans.Class (lift)

-- Basic Instructions for our ServerM programs --

getSource :: FilePath -> ServerM (Maybe Text)
getSource filepath = fmap LSP.virtualFileText
                      <$> LSP.getVirtualFile (LSP.toNormalizedUri (LSP.filePathToUri filepath))

logText :: Text -> ServerM ()
logText = Server.Monad.logText

bumpVersion :: ServerM Int
bumpVersion = Server.Monad.bumpVersion

sendDiagnostics :: FilePath -> [LSP.Diagnostic] -> ServerM ()
sendDiagnostics = Server.Monad.sendDiagnosticsLSP

editText :: Range -> Text -> ServerM () -> ServerM ()
editText range textToReplace onSuccess = do
  let requestParams :: LSP.ApplyWorkspaceEditParams
        = LSP.ApplyWorkspaceEditParams {
            _label = Just "Resolve Spec",
            _edit = LSP.WorkspaceEdit {
              _changes = Nothing,
              _documentChanges = Just (LSP.List [LSP.InL textDocumentEdit]),
              _changeAnnotations = Nothing
            }
          }
  _ <- LSP.sendRequest LSP.SWorkspaceApplyEdit requestParams (const onSuccess)
  return ()

  where
    filepath :: FilePath
    filepath = rangeFile range
    textDocumentEdit :: LSP.TextDocumentEdit
    textDocumentEdit = LSP.TextDocumentEdit {
      _textDocument = LSP.VersionedTextDocumentIdentifier (LSP.filePathToUri filepath) (Just 0),
      _edits = LSP.List [LSP.InL textEdit]
    }
    textEdit :: LSP.TextEdit
    textEdit = LSP.TextEdit {
      _range = SrcLoc.toLSPRange range,
      _newText = textToReplace
    }

cacheProgram :: FilePath -> LoadedProgram -> ServerM LoadedProgram
cacheProgram filepath loadedProgram = do
  ref <- lift $ asks loadedPrograms
  liftIO $ modifyIORef' ref (Map.insert filepath loadedProgram)
  return loadedProgram

dumpProgram :: FilePath -> ServerM (Maybe LoadedProgram)
dumpProgram filepath = do
  ref     <- lift $ asks loadedPrograms
  mapping <- liftIO $ readIORef ref
  case Map.lookup filepath mapping of
    Nothing    -> return Nothing
    Just loadedProgram -> return $ Just loadedProgram