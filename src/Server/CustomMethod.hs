{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}

module Server.CustomMethod where

import Control.Monad.Cont
import Control.Monad.Except
import Control.Monad.Reader
import Control.Monad.State
import Control.Monad.Trans.Free
import Data.IORef
import Data.List (find, sort)
import Data.Loc
import Data.Loc.Range
import qualified Data.Map as Map
import Data.Text (Text)
import qualified Data.Text as Text
import Error
import GCL.Expr (expand, runSubstM)
import qualified GCL.Type as TypeChecking
import GCL.WP (StructWarning)
import qualified GCL.WP as WP
import Language.LSP.Server
import Language.LSP.Types hiding (Range, TextDocumentSyncClientCapabilities)
import qualified Language.LSP.VFS as VFS
import Server.Diagnostic
import Server.DiffMap (DiffMap)
import qualified Server.DiffMap as DiffMap
import Server.Monad
import qualified Syntax.Abstract as A
import Syntax.Parser (Parser, pProgram, pStmts, runParse)
import Syntax.Predicate (PO, Spec (specLoc), specPayload)
import Prelude hiding (span)
import Syntax.Concrete.ToAbstract
import Control.Monad.Writer

--------------------------------------------------------------------------------

data Eff next
  = EditText Range Text (Text -> next)
  | UpdateSavedSource Text next
  | ReadSavedSource (Text -> next)
  | ReadLatestSource (Text -> next)
  | UpdateLastMouseSelection (Int, Int) next
  | ReadLastMouseSelection (Maybe (Int, Int) -> next)
  | BumpResponseVersion (Int -> next)
  | Log Text next
  | Terminate [ResKind] [Diagnostic]
  deriving (Functor)

-- for testing 
data EffKind 
  = EffEditText Range Text
  | EffUpdateSavedSource Text
  | EffReadSavedSource
  | EffReadLatestSource
  | EffUpdateLastMouseSelection (Int, Int)
  | EffReadLastMouseSelection 
  | EffBumpResponseVersion 
  | EffLog Text
  | EffTerminate [ResKind] [Diagnostic]
  deriving (Eq, Show)

data EffEnv = EffEnv
  { effEnvFilePath :: FilePath,
    effEnvResponder :: Maybe Responder
  }

type EffM = FreeT Eff (ExceptT Error (ReaderT EffEnv (State DiffMap)))

runEffM :: EffEnv -> EffM a -> Either Error (FreeF Eff a (EffM a))
runEffM env p = evalState (runReaderT (runExceptT (runFreeT p)) env) DiffMap.empty

handleError :: FilePath -> Maybe Responder -> Error -> ServerM ()
handleError filepath responder err = do
  let responses = [ResError [globalError err]]
  let diagnostics = toDiagnostics err
  -- send diagnostics
  sendDiagnostics filepath diagnostics
  -- send responses
  sendResponses filepath responder responses

interpret :: EffEnv -> EffM () -> ServerM ()
interpret env@(EffEnv filepath responder) p = case runEffM env p of
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
            newSource <- latestSource
            updateSavedSource newSource
            -- update the offset diff map
            let offset = posCoff (rangeStart range)
            let diff = Text.length text - span range
            modify' (DiffMap.insert offset diff)

            next newSource

    void $ sendRequest SWorkspaceApplyEdit applyWorkspaceEditParams callback
  Right (Free (ReadSavedSource next)) -> do
    ref <- lift $ asks envSourceMap
    mapping <- liftIO $ readIORef ref
    let result = fst <$> Map.lookup filepath mapping
    case result of
      Nothing -> handleError filepath responder (CannotReadFile filepath)
      Just source -> do
        logText "ReadSavedSource"
        interpret env (next source)
  Right (Free (UpdateSavedSource source next)) -> do
    ref <- lift $ asks envSourceMap
    liftIO $ modifyIORef' ref (Map.insertWith (\(src, _) (_, sel) -> (src, sel)) filepath (source, Nothing))
    interpret env next
  Right (Free (ReadLatestSource next)) -> do
    result <- fmap VFS.virtualFileText <$> getVirtualFile (toNormalizedUri (filePathToUri filepath))
    case result of
      Nothing -> handleError filepath responder (CannotReadFile filepath)
      Just source -> do
        logText "ReadLatestSource"
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
    ref <- lift $ asks envCounter
    n <- liftIO $ readIORef ref
    liftIO $ writeIORef ref (succ n)
    interpret env (next n)
  Right (Free (Log text next)) -> do
    logText text
    interpret env next
  Right (Free (Terminate responses diagnostics)) -> do
    -- send diagnostics
    sendDiagnostics filepath diagnostics
    -- send responses
    sendResponses filepath responder responses
  Left err -> do
    logStuff err
    handleError filepath responder err


runTest :: EffM a -> (Maybe a, [EffKind])
runTest program = runWriter (interpret2 program)

interpret2 :: EffM a -> Writer [EffKind] (Maybe a)
interpret2 p = case runEffM (EffEnv "<test>" Nothing) p of
  Right (Pure a) -> return (Just a)
  Right (Free (EditText range text next)) -> do
    tell [EffEditText range text]
    interpret2 (next "")
  Right (Free (ReadSavedSource next)) -> do
    tell [EffReadSavedSource]
    interpret2 (next "")
  Right (Free (UpdateSavedSource source next)) -> do
    tell [EffUpdateSavedSource source]
    interpret2 next
  Right (Free (ReadLatestSource next)) -> do
    tell [EffReadLatestSource]
    interpret2 (next "")
  Right (Free (ReadLastMouseSelection next)) -> do
    tell [EffReadLastMouseSelection]
    interpret2 (next Nothing)
  Right (Free (UpdateLastMouseSelection selection next)) -> do
    tell [EffUpdateLastMouseSelection selection]
    interpret2 next
  Right (Free (BumpResponseVersion next)) -> do
    tell [EffBumpResponseVersion]
    interpret2 (next 0)
  Right (Free (Log text next)) -> do
    tell [EffLog text]
    interpret2 next
  Right (Free (Terminate responses diagnostics)) -> do
    -- undefined 
    tell [EffTerminate responses diagnostics]
    return Nothing
  Left err -> do
    let responses = [ResError [globalError err]]
    let diagnostics = toDiagnostics err
    tell [EffTerminate responses diagnostics]
    return Nothing


editText :: Range -> Text -> EffM Text
editText range text = liftF (EditText range text id)

savedSource :: EffM Text
savedSource = liftF (ReadSavedSource id)

updateSavedSource :: Text -> EffM ()
updateSavedSource source = liftF (UpdateSavedSource source ())

latestSource :: EffM Text
latestSource = liftF (ReadLatestSource id)

updateLastMouseSelection :: (Int, Int) -> EffM ()
updateLastMouseSelection selection = liftF (UpdateLastMouseSelection selection ())

readLastMouseSelection :: EffM (Maybe (Int, Int))
readLastMouseSelection = liftF (ReadLastMouseSelection id)

logM :: Text -> EffM ()
logM text = liftF (Log text ())

bumpVersion :: EffM Int
bumpVersion = liftF (BumpResponseVersion id)

terminate :: [ResKind] -> [Diagnostic] -> EffM ()
terminate x y = liftF (Terminate x y)

------------------------------------------------------------------------------

-- converts the "?" at a given location to "[!   !]"
-- and returns the modified source and the difference of source length
digHole :: Pos -> EffM Text
digHole pos = do
  let indent = Text.replicate (posCol pos - 1) " "
  let holeText = "[!\n" <> indent <> "\n" <> indent <> "!]"
  editText (Range pos pos) holeText

-- | Try to parse a piece of text in a Spec
refine :: Text -> (Int, Int) -> EffM (Spec, Text)
refine source (start, end) = do
  result <- findPointedSpec
  case result of
    Nothing -> throwError $ Others "Please place the cursor in side a Spec to refine it"
    Just spec -> do
      source' <- latestSource
      let payload = Text.unlines $ specPayload source' spec
      -- HACK, `pStmts` will kaput if we feed empty strings into it
      let payloadIsEmpty = Text.null (Text.strip payload)
      if payloadIsEmpty
        then return ()
        else void $ parse pStmts payload
      return (spec, payload)
  where
    findPointedSpec :: EffM (Maybe Spec)
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

--
substitute :: A.Program -> A.Expr -> A.Subst -> A.Expr
substitute (A.Program _ _ defns _ _) expr subst =
  runSubstM (expand (A.Subst expr subst)) defns 1

typeCheck :: A.Program -> EffM ()
typeCheck p = case runExcept (TypeChecking.checkProg p) of
  Left e -> throwError $ TypeError e
  Right v -> return v

sweep :: A.Program -> EffM ([PO], [Spec], [A.Expr], [StructWarning])
sweep program@(A.Program _ globalProps _ _ _) =
  case WP.sweep program of
    Left e -> throwError $ StructError e
    Right (pos, specs, warings) -> do
      return (pos, specs, globalProps, warings)

--------------------------------------------------------------------------------

adjustOffset :: Int -> EffM Int
adjustOffset offset = do
  diffMap <- get
  return $ DiffMap.adjust diffMap offset

-- | Parse with a parser
parse :: Parser a -> Text -> EffM a
parse p source = do
  filepath <- asks effEnvFilePath
  case runParse p filepath source of
    Left err -> throwError (SyntacticError err)
    Right val -> return val

parseProgram :: Text -> EffM A.Program
parseProgram source = do
  concrete <- parse pProgram source
  case runExcept (toAbstract concrete) of
    Left NoLoc -> throwError $ Others "NoLoc in parseProgram"
    Left (Loc start _) -> digHole start >>= parseProgram
    Right program -> return program

-- | Given an interval of mouse selection, calculate POs within the interval, ordered by their vicinity
filterPOs :: (Int, Int) -> [PO] -> [PO]
filterPOs (selStart, selEnd) pos = opverlappedPOs
  where
    opverlappedPOs = reverse $ case overlapped of
      [] -> []
      (x : _) -> case locOf x of
        NoLoc -> []
        Loc start _ ->
          let same y = case locOf y of
                NoLoc -> False
                Loc start' _ -> start == start'
           in filter same overlapped
      where
        -- find the POs whose Range overlaps with the selection
        isOverlapped po = case locOf po of
          NoLoc -> False
          Loc start' end' ->
            let start = posCoff start'
                end = posCoff end' + 1
             in (selStart <= start && selEnd >= start) -- the end of the selection overlaps with the start of PO
                  || (selStart <= end && selEnd >= end) -- the start of the selection overlaps with the end of PO
                  || (selStart <= start && selEnd >= end) -- the selection covers the PO
                  || (selStart >= start && selEnd <= end) -- the selection is within the PO
                  -- sort them by comparing their starting position
        overlapped = reverse $ sort $ filter isOverlapped pos
