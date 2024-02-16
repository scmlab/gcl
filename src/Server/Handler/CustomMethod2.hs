{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Server.Handler.CustomMethod2 where

import qualified Server.Monad (logText, sendDiagnosticsLSP)

import qualified Language.LSP.Types as LSP
import qualified Syntax.Parser (scanAndParse)

handler :: JSON.Value -> (Response -> ServerM ()) -> ServerM ()
handler params responder = do
    -- JSON Value => Request => Response
  case JSON.fromJSON params of
    JSON.Error msg -> do
      -- logText
      --   $  " --> CustomMethod: CannotDecodeRequest "
      --   <> Text.pack (show msg)
      --   <> " "
      --   <> Text.pack (show params)
      responder $ CannotDecodeRequest $ show msg ++ "\n" ++ show params
    JSON.Success request -> handleRequest request
  where
    -- convert Request to Response and Diagnostics 
    handleRequest :: Request -> ServerM ()
    handleRequest request@(Req filePath reqKind) = do
      case reqKind of
        ReqReload           -> handleReload filePath
        ReqRefine range     -> handleRefine filePath range
        ReqHelloworld range -> replaceWithHelloworld' range
        _                   -> return $ error "Not yet implemented"

handleRefine :: FilePath -> Range -> ServerM ()
handleRefine filePath range = _

-- Basic Instructions for our ServerM programs --

getSource :: Filepath -> ServerM (Maybe Text)
getSource = fmap LSP.virtualFileText
              <$> LSP.getVirtualFile (LSP.toNormalizedUri (LSP.filePathToUri filepath))
-- getSource = fmap J.virtualFileText
--             <$> J.getVirtualFile
--             $ J.toNormalizedUri
--             $ J.filePathToUri filepath

logText :: Text -> ServerM ()
logText = Server.Monad.logText

sendDiagnostics :: Filepath -> LSP.Diagnostic -> ServerM ()
sendDiagnostics = Server.Monad.sendDiagnosticsLSP

editText :: Range -> Text -> ServerM () -> ServerM ()
editText range textToReplace onSuccess = do
  let requestParams :: LSP.ApplyWorkspaceEditParams
    = LSP.ApplyWorkspaceEditParams {
      _label = (Just "Resolve Spec"),
      _edit = LSP.WorkspaceEdit {
        _changes = Nothing,
        _documentChanges = Just (LSP.List [LSP.InL textDocumentEdit]),
        _changeAnnotations = Nothing
      }
    }
  _ <- LSP.sendRequest LSP.SWorkspaceApplyEdit requestParams (\_ -> onSuccess)
  return ()

  where
    textDocumentEdit :: LSP.TextDocumentEdit
    textDocumentEdit = LSP.TextDocumentEdit {
      _textDocument = J.VersionedTextDocumentIdentifier (LSP.filePathToUri filepath) (Just 0),
      _edits = LSP.List [LSP.InL textEdit]
    }
    textEdit :: LSP.TextEdit
    textEdit = LSP.TextEdit {
      _range = (SrcLoc.toLSPRange range),
      _newText = textToReplace
    }

-- ServerM program - example 1 --

replaceWithHelloworld :: Filepath -> Range -> ServerM ()
replaceWithHelloworld filepath range = do
  source <- getSource filepath
  logText "before replacement"
  logText source
  logText "\n"
  editText range "Hello, World!" $ do
    source <- getSource filepath
    logText "after replacement"
    logText source
    logText "\n"

replaceWithHelloworld' :: Range -> ServerM ()
replaceWithHelloworld' range = do
  let filepath :: Filepath = rangeFile range
  replaceWithHelloworld filepath range


-- Reload --

handleReload :: FilePath -> ServerM ()
handleReload filePath = do
  source <- getSource filepath
  abstract <- case parse filepath source of
    Left error -> _
    Right concrete -> convert filepath concrete

parse :: Filepath -> Text -> Either ParseError C.Program
parse filepath source =
  case Parser.scanAndParse Parser.program filepath source of
    Left  error   -> Left [LexicalError error]
    Right program -> Right program

convert :: Filepath -> C.Program -> ServerM A.Program
convert filepath concrete = do
  case runExcept $ toAbstract concrete of
    Left rangeToDigHole -> do
      digHole rangeToDigHole
      result' <- parse filepath
      convert filepath result'
    Right abstract -> return abstract
  where
    digHole :: Filepath -> Range -> ServerM ()
    digHole filepath range = do
      logText $ "    < DigHole " <> toText range
      let indent   = Text.replicate (posCol (rangeStart range) - 1) " "
      let holeText = "[!\n" <> indent <> "\n" <> indent <> "!]"
      editText range holeText (return ())

-- runTypeCheck
--   :: Program -> Either TypeError (ScopeTreeZipper TypeDefnInfo, ScopeTreeZipper Type)

-- typeCheck :: ConvertResult -> PipelineM TypeCheckResult
-- typeCheck result = do
--   let program = convertedProgram result

--   (_, scopeTree) <- case TypeChecking.runTypeCheck program of
--     Left  e -> throwError [TypeError e]
--     Right v -> return v

--   let typeChecked = TypeCheckResult
--         { typeCheckedPreviousStage = result
--         , typeCheckedIntervalMap   = collectHoverInfo program scopeTree
--         }
--   save (TypeChecked typeChecked) -- save the current progress
--   return typeChecked

