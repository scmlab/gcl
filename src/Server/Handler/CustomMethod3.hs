{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}


import qualified Data.Aeson as JSON
import Server.CustomMethod
import qualified Language.LSP.Server as LSP
import Control.Monad.Trans.Reader (ReaderT (runReaderT), asks)
import Control.Monad.Trans.Except (ExceptT, runExceptT, runExcept)
import Error (Error (CannotReadFile, ParseError))
import Data.Loc.Range (Range, rangeFile, rangeStart)
import Data.Text (Text)
import qualified Language.LSP.VFS as LSP
import qualified Language.LSP.Types as LSP
import qualified Server.SrcLoc as SrcLoc
import qualified Syntax.Parser as Parser
import qualified Syntax.Concrete as C
import qualified Syntax.Abstract as A
import qualified Data.Text as Text
import Control.Monad.Trans (lift, liftIO)
import Control.Concurrent.Chan (writeChan, Chan)
import qualified Language.LSP.Diagnostics as LSP
import Data.IORef (IORef, readIORef, writeIORef)
import Control.Monad.Except (throwError)
import Syntax.Concrete (ToAbstract(..))
import Pretty.Util (toText)
import Data.Loc (posCol)
import Language.LSP.VFS (VirtualFile(VirtualFile))

data GlobalEnv = GlobalEnv
  { -- Channel for printing log
    globalChan         :: Chan Text
  ,
    -- Counter for generating fresh numbers
    globalCounter      :: IORef Int
  }

type ServerM = LSP.LspT () (ReaderT GlobalEnv (ExceptT Error IO))

runServerM :: ServerM a -> GlobalEnv -> LSP.LanguageContextEnv () -> IO (Either Error a)
runServerM program env ctxEnv = runExceptT $ runReaderT (LSP.runLspT ctxEnv program) env

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
    handleRequest _request@(Req filePath reqKind) = do
      case reqKind of
        ReqReload           -> handleReload filePath
        ReqRefine range     -> handleRefine filePath range
        ReqHelloWorld range -> replaceWithHelloworld' range
        _                   -> return $ error "Not yet implemented"

handleRefine :: FilePath -> Range -> ServerM ()
handleRefine filePath range = _

-- Basic Instructions for our ServerM programs --

getSource :: FilePath -> ServerM Text
getSource filepath = do
  -- let y = LSP.toNormalizedUri (LSP.filePathToUri filepath)
  -- let z :: ServerM (Maybe VirtualFile) = LSP.getVirtualFile y
  -- let x = LSP.getVirtualFile (LSP.toNormalizedUri (LSP.filePathToUri filepath))
  _
    -- result <- fmap LSP.virtualFileText
    --   <$> LSP.getVirtualFile (LSP.toNormalizedUri (LSP.filePathToUri filepath))
    -- case result of
    --   Nothing     -> throwError (CannotReadFile filepath)
    --   Just source -> executeOneStep filepath continuation (next source)
    -- case maybeVirtualFile of
    --     Nothing -> throwError (CannotReadFile filepath)
    --     Just virtualFile -> return $ LSP.virtualFileText virtualFile

logText :: Text -> ServerM ()
logText s = do
  chan <- lift $ asks globalChan
  liftIO $ writeChan chan s

sendDiagnostics :: FilePath -> [LSP.Diagnostic] -> ServerM ()
sendDiagnostics filepath diagnostics = do
  version <- bumpVersion
  -- LSP.publishDiagnostics 100
  --                      (LSP.toNormalizedUri (LSP.filePathToUri filepath))
  --                      (Just version)
  --                      (LSP.partitionBySource diagnostics)

bumpVersion :: ServerM Int
bumpVersion = do
  ref <- lift $ asks globalCounter
  n   <- liftIO $ readIORef ref
  liftIO $ writeIORef ref (succ n)
  return n

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

-- ServerM program - example 1 --

replaceWithHelloworld :: FilePath -> Range -> ServerM ()
replaceWithHelloworld filepath range = do
  source <- getSource filepath
  logText "before replacement"
  logText source
  logText "\n"
  editText range "Hello, World!" $ do
    source' <- getSource filepath
    logText "after replacement"
    logText source'
    logText "\n"

replaceWithHelloworld' :: Range -> ServerM ()
replaceWithHelloworld' range = do
  let filepath :: FilePath = rangeFile range
  replaceWithHelloworld filepath range


-- Reload --

handleReload :: FilePath -> ServerM ()
handleReload filepath = do
  source <- getSource filepath
  concrete <- parse filepath source
  convert filepath concrete (\abstract -> do
      -- type check
      return ()
    )

parse :: FilePath -> Text -> ServerM C.Program
parse filepath source =
  case Parser.scanAndParse Parser.program filepath source of
    Left  err   -> throwError (ParseError err)
    Right program -> return program

convert :: FilePath -> C.Program -> (A.Program -> ServerM ()) -> ServerM ()
convert filepath concrete onFinish = do
  case runExcept $ toAbstract concrete of
    Left rangeToDigHole -> do
      digHole rangeToDigHole do
        sourceAfterHoleDigging <- getSource filepath
        concreteAfterHoleDigging <- parse filepath sourceAfterHoleDigging
        convert filepath concreteAfterHoleDigging onFinish
    Right abstract -> onFinish abstract
  where
    digHole :: Range -> ServerM () -> ServerM ()
    digHole range _onFinish = do
      logText $ "    < DigHole " <> toText range
      let indent   = Text.replicate (posCol (rangeStart range) - 1) " "
      let holeText = "[!\n" <> indent <> "\n" <> indent <> "!]"
      editText range holeText _onFinish