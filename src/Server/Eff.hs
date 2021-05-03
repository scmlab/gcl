{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}

module Server.Eff where

import Control.Monad.Cont
import Control.Monad.Except
import Control.Monad.Reader
import Control.Monad.Trans.Free
import Data.List ( sort, find )
import Data.Loc
import Data.Text (Text)
import qualified Data.Text as Text
import Error
import qualified GCL.Type as TypeChecking
import GCL.WP (StructWarning)
import Language.LSP.Server
import Language.LSP.Types hiding (TextDocumentSyncClientCapabilities)
import Server.Diagnostic
import Server.Monad
import qualified Syntax.Abstract as A
import Syntax.Concrete (toAbstract)
import Syntax.Parser (Parser, pProgram, runParse, pStmts)
import Syntax.Predicate ( Spec (specLoc), PO, specPayload )
import qualified Data.Map as Map
import Data.IORef
import qualified GCL.WP as WP
import GCL.Expr (runSubstM, expand)
import qualified Language.LSP.VFS as VFS

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

data EffEnv = EffEnv
  { effEnvFilePath :: FilePath,
    effEnvResponder :: Maybe Responder
  }

type EffM = FreeT Eff (ExceptT Error (Reader EffEnv))

runEffM :: EffEnv -> EffM a -> Either Error (FreeF Eff a (EffM a))
runEffM env p = runReader (runExceptT (runFreeT p)) env

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
    let removeSpec = TextEdit range text
    let identifier = VersionedTextDocumentIdentifier (filePathToUri filepath) (Just 0)
    let textDocumentEdit = TextDocumentEdit identifier (List [InL removeSpec])
    let change = InL textDocumentEdit
    let workspaceEdit = WorkspaceEdit Nothing (Just (List [change])) Nothing
    let applyWorkspaceEditParams = ApplyWorkspaceEditParams (Just "Resolve Spec") workspaceEdit
    let callback _ = do
          logText "After EditText"
          interpret env $ do 
            -- update saved source after text editting 
            newSource <- latestSource
            updateSavedSource newSource 
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

logM :: Show a => a -> EffM ()
logM text = liftF (Log (Text.pack (show text)) ())

bumpVersion :: EffM Int
bumpVersion = liftF (BumpResponseVersion id)

terminate :: [ResKind] -> [Diagnostic] -> EffM ()
terminate x y = liftF (Terminate x y)


------------------------------------------------------------------------------

digHole :: Loc -> EffM ()
digHole loc = do
  filepath <- asks effEnvFilePath
  case loc of
    NoLoc -> throwError (CannotReadFile filepath)
    Loc start _end -> do
      let indent = Text.replicate (posCol start - 1) " "
      let holeText = "[!\n" <> indent <> "\n" <> indent <> "!]"
      void $ editText (locToRange loc) holeText

-- | Try to parse a piece of text in a Spec
refine :: Text -> (Int, Int) -> EffM (Spec, Text)
refine source selection = do
  result <- findPointedSpec
  case result of
    Nothing -> throwError $ Others "Please place the cursor in side a Spec to refine it"
    Just spec -> do
      let payload = Text.unlines $ specPayload source spec
      -- HACK 
      let payloadIsEmpty = Text.null (Text.strip payload)
      if payloadIsEmpty
        then return ()
        else void $ parse pStmts payload
      return (spec, payload)
  where
    findPointedSpec :: EffM (Maybe Spec)
    findPointedSpec = do
      (_, specs, _, _) <- parseProgram source >>= sweep
      return $ find (pointed selection) specs
      where
        pointed :: (Int, Int) -> Spec -> Bool
        pointed (start, end) spec = case specLoc spec of
          NoLoc -> False
          Loc open close ->
            (posCoff open <= start && start <= posCoff close + 1)
              || (posCoff open <= end && end <= posCoff close + 1)

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

-- | Parse with a parser
parse :: Parser a -> Text -> EffM a
parse p source = do
  filepath <- asks effEnvFilePath
  case runParse p filepath source of
    Left err -> throwError (SyntacticError err)
    Right val -> return val

-- | Parse the whole program
parseProgram :: Text -> EffM A.Program
parseProgram source = do
  concrete <- parse pProgram source
  case runExcept (toAbstract concrete) of
    Left loc -> do
      digHole loc
      latestSource >>= parseProgram
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
