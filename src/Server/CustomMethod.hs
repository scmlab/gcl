{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}

module Server.CustomMethod where

import Control.Monad.Cont
import Control.Monad.Except
import Control.Monad.Reader
import Control.Monad.State
import Control.Monad.Trans.Free
import Control.Monad.Writer
import Data.IORef
import Data.List (find, sortOn)
import Data.Loc
import Data.Loc.Range
import qualified Data.Map as Map
import Data.Text (Text)
import qualified Data.Text as Text
import Error
import qualified GCL.Type as TypeChecking
import GCL.WP (StructWarning)
import qualified GCL.WP as WP
import Language.LSP.Server
import Language.LSP.Types hiding (Range, TextDocumentSyncClientCapabilities)
import qualified Language.LSP.VFS as VFS
import Render
import Server.Diagnostic
import Server.DiffMap (DiffMap)
import qualified Server.DiffMap as DiffMap
import Server.Monad
import qualified Syntax.Abstract as A
import Syntax.Concrete.ToAbstract
import Syntax.Parser (Parser, pProgram, pStmts, runParse)
import Syntax.Predicate (PO, Spec (specLoc), specPayload)
import Prelude hiding (span)

--------------------------------------------------------------------------------

-- The "Syntax" of the DSL for handling LSP requests and responses
data Cmd next
  = EditText Range Text (Text -> next)
  | ReadSource (Text -> next)
  | UpdateLastMouseSelection (Int, Int) next
  | ReadLastMouseSelection (Maybe (Int, Int) -> next)
  | BumpResponseVersion (Int -> next)
  | Log Text next
  | Terminate [ResKind] [Diagnostic]
  deriving (Functor)

data CmdEnv = CmdEnv
  { cmdEnvFilePath :: FilePath,
    cmdEnvResponder :: Maybe Responder
  }

type CmdM = FreeT Cmd (ExceptT [Error] (ReaderT CmdEnv (State DiffMap)))

runCmdM :: CmdEnv -> CmdM a -> Either [Error] (FreeF Cmd a (CmdM a))
runCmdM env p = evalState (runReaderT (runExceptT (runFreeT p)) env) DiffMap.empty

editText :: Range -> Text -> CmdM Text
editText range text = liftF (EditText range text id)

readSource :: CmdM Text
readSource = liftF (ReadSource id)

updateLastMouseSelection :: (Int, Int) -> CmdM ()
updateLastMouseSelection selection = liftF (UpdateLastMouseSelection selection ())

readLastMouseSelection :: CmdM (Maybe (Int, Int))
readLastMouseSelection = liftF (ReadLastMouseSelection id)

logM :: Text -> CmdM ()
logM text = liftF (Log text ())

bumpVersion :: CmdM Int
bumpVersion = liftF (BumpResponseVersion id)

terminate :: [ResKind] -> [Diagnostic] -> CmdM ()
terminate x y = liftF (Terminate x y)

------------------------------------------------------------------------------

-- converts the "?" at a given location to "[!   !]"
-- and returns the modified source and the difference of source length
digHole :: Pos -> CmdM Text
digHole pos = do
  let indent = Text.replicate (posCol pos - 1) " "
  let holeText = "[!\n" <> indent <> "\n" <> indent <> "!]"
  editText (Range pos pos) holeText

-- | Try to parse a piece of text in a Spec
refine :: Text -> (Int, Int) -> CmdM (Spec, Text)
refine source (start, end) = do
  result <- findPointedSpec
  case result of
    Nothing -> throwError [Others "Please place the cursor in side a Spec to refine it"]
    Just spec -> do
      source' <- readSource
      let payload = Text.unlines $ specPayload source' spec
      -- HACK, `pStmts` will kaput if we feed empty strings into it
      let payloadIsEmpty = Text.null (Text.strip payload)
      if payloadIsEmpty
        then return ()
        else void $ parse pStmts payload
      return (spec, payload)
  where
    findPointedSpec :: CmdM (Maybe Spec)
    findPointedSpec = do
      program <- parseProgram source
      (_, specs, _, _) <- sweep program
      -- adjust offsets of selections
      start' <- adjustOffset start
      end' <- adjustOffset end
      logM $ Text.pack $ show (start, start')
      logM $ Text.pack $ show (end, end')

      return $ find (pointed (start', end')) specs
      where
        pointed :: (Int, Int) -> Spec -> Bool
        pointed (x, y) spec = case specLoc spec of
          NoLoc -> False
          Loc open close ->
            (posCoff open <= x && x <= posCoff close + 1)
              || (posCoff open <= y && y <= posCoff close + 1)

typeCheck :: A.Program -> CmdM ()
typeCheck p = case runExcept (TypeChecking.checkProg p) of
  Left e -> throwError [TypeError e]
  Right v -> return v

sweep :: A.Program -> CmdM ([PO], [Spec], [A.Expr], [StructWarning])
sweep program@(A.Program _ globalProps _ _ _) =
  case WP.sweep program of
    Left e -> throwError [StructError e]
    Right (pos, specs, warings) -> do
      return (sortOn locOf pos, sortOn locOf specs, globalProps, warings)

--------------------------------------------------------------------------------

adjustOffset :: Int -> CmdM Int
adjustOffset offset = do
  diffMap <- get
  return $ DiffMap.adjust diffMap offset

-- | Parse with a parser
parse :: Parser a -> Text -> CmdM a
parse p source = do
  filepath <- asks cmdEnvFilePath
  case runParse p filepath source of
    Left errors -> throwError $ map SyntacticError errors
    Right val -> return val

parseProgram :: Text -> CmdM A.Program
parseProgram source = do
  concrete <- parse pProgram source
  case runExcept (toAbstract concrete) of
    Left NoLoc -> throwError [Others "NoLoc in parseProgram"]
    Left (Loc start _) -> digHole start >>= parseProgram
    Right program -> return program

-- | Compare the mouse position with something
--  EQ: the mouse is placed within that thing
--  LT: the mouse is placed BEFORE (but not touching) that thing
--  GT: the mouse is placed AFTER (but not touching) that thing
compareWithMousePosition :: Located a => Int -> a -> Ordering
compareWithMousePosition offset x = case locOf x of
  NoLoc -> EQ
  Loc start end ->
    if offset < posCoff start
      then LT
      else
        if offset - 1 > posCoff end
          then GT
          else EQ

-- | See if something is within the mouse selection
withinMouseSelection :: Located a => (Int, Int) -> a -> Bool
withinMouseSelection (left, right) x =
  compareWithMousePosition left x == EQ
    || compareWithMousePosition right x == EQ
    || (compareWithMousePosition left x == LT && compareWithMousePosition right x == GT)

--------------------------------------------------------------------------------

handleErrors :: FilePath -> Maybe Responder -> [Error] -> ServerM ()
handleErrors filepath responder errors = do
  version <- bumpVersionM
  -- (IdInt version)
  let responses = [ResDisplay version (headerE "Errors" : map renderBlock errors)]
  let diagnostics = errors >>= toDiagnostics
  -- send diagnostics
  sendDiagnostics filepath diagnostics
  -- send responses
  sendResponses filepath responder responses

bumpVersionM :: ServerM Int
bumpVersionM = do
  ref <- lift $ asks envCounter
  n <- liftIO $ readIORef ref
  liftIO $ writeIORef ref (succ n)
  return n

interpret :: CmdEnv -> CmdM () -> ServerM ()
interpret env@(CmdEnv filepath responder) p = case runCmdM env p of
  Right (Pure ()) -> logText "Improper termination"
  Right (Free (EditText range text next)) -> do
    logText "Before EditText"
    -- apply edit
    let removeSpec = TextEdit (rangeToRange range) text
    let identifier = VersionedTextDocumentIdentifier (filePathToUri filepath) (Just 0)
    let textDocumentEdit = TextDocumentEdit identifier (List [InL removeSpec])
    let change = InL textDocumentEdit
    let workspaceEdit = WorkspaceEdit Nothing (Just (List [change])) Nothing
    let applyWorkspaceEditParams = ApplyWorkspaceEditParams (Just "Resolve Spec") workspaceEdit
    let callback _ = do
          logText "After EditText"
          interpret env $ do
            -- update saved source
            newSource <- readSource
            -- update the offset diff map
            let offset = posCoff (rangeStart range)
            let diff = Text.length text - span range
            modify' (DiffMap.insert offset diff)

            next newSource

    void $ sendRequest SWorkspaceApplyEdit applyWorkspaceEditParams callback
  Right (Free (ReadSource next)) -> do
    result <- fmap VFS.virtualFileText <$> getVirtualFile (toNormalizedUri (filePathToUri filepath))
    case result of
      Nothing -> handleErrors filepath responder [CannotReadFile filepath]
      Just source -> do
        logText "ReadSource"
        interpret env (next source)
  Right (Free (ReadLastMouseSelection next)) -> do
    ref <- lift $ asks envSourceMap
    mapping <- liftIO $ readIORef ref
    let selection = snd =<< Map.lookup filepath mapping
    interpret env (next selection)
  Right (Free (UpdateLastMouseSelection selection next)) -> do
    ref <- lift $ asks envSourceMap
    liftIO $ modifyIORef' ref (Map.update (\(source, _) -> Just (source, Just selection)) filepath)
    interpret env next
  Right (Free (BumpResponseVersion next)) -> do
    n <- bumpVersionM
    interpret env (next n)
  Right (Free (Log text next)) -> do
    logText text
    interpret env next
  Right (Free (Terminate responses diagnostics)) -> do
    -- send diagnostics
    sendDiagnostics filepath diagnostics
    -- send responses
    sendResponses filepath responder responses
  Left errors -> do
    logStuff errors
    handleErrors filepath responder errors

--------------------------------------------------------------------------------

-- Semantics for testing

data CmdKind
  = CmdEditText Range Text
  | CmdReadSource
  | CmdUpdateLastMouseSelection (Int, Int)
  | CmdReadLastMouseSelection
  | CmdBumpResponseVersion
  | CmdLog Text
  | CmdTerminate [ResKind] [Diagnostic]
  deriving (Eq, Show)

runTest :: CmdM a -> (Maybe a, [CmdKind])
runTest program = runWriter (interpret2 program)

interpret2 :: CmdM a -> Writer [CmdKind] (Maybe a)
interpret2 p = case runCmdM (CmdEnv "<test>" Nothing) p of
  Right (Pure a) -> return (Just a)
  Right (Free (EditText range text next)) -> do
    tell [CmdEditText range text]
    interpret2 (next "")
  Right (Free (ReadSource next)) -> do
    tell [CmdReadSource]
    interpret2 (next "")
  Right (Free (ReadLastMouseSelection next)) -> do
    tell [CmdReadLastMouseSelection]
    interpret2 (next Nothing)
  Right (Free (UpdateLastMouseSelection selection next)) -> do
    tell [CmdUpdateLastMouseSelection selection]
    interpret2 next
  Right (Free (BumpResponseVersion next)) -> do
    tell [CmdBumpResponseVersion]
    interpret2 (next 0)
  Right (Free (Log text next)) -> do
    tell [CmdLog text]
    interpret2 next
  Right (Free (Terminate responses diagnostics)) -> do
    -- undefined
    tell [CmdTerminate responses diagnostics]
    return Nothing
  Left errors -> do
    let responses = [ResDisplay 0 (headerE "Errors" : map renderBlock errors)]
    let diagnostics = errors >>= toDiagnostics
    tell [CmdTerminate responses diagnostics]
    return Nothing
