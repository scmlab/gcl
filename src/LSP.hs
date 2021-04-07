{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PolyKinds #-}

module LSP where

-- import Control.Monad.IO.Class

import Control.Concurrent (forkIO)
import Control.Concurrent.Chan (Chan, newChan, readChan, writeChan)
import Control.Monad.Except hiding (guard)
import Control.Monad.Reader
import Data.Aeson (FromJSON, ToJSON)
import qualified Data.Aeson as JSON
import Data.IORef (IORef, newIORef, readIORef, modifyIORef', writeIORef)
import Data.List (sort)
import Data.Loc (Loc (..), Located (locOf), Pos (..), posCoff, posFile)
import Data.Text (Text, pack)
import qualified Data.Text as Text
import qualified Data.Text.IO as Text
import qualified Data.Text.Lazy as LazyText
import Error
import GCL.Expr (expand, runSubstM)
import GCL.Type (TypeError (..))
import qualified GCL.Type as TypeChecking
import GCL.WP (StructError (..), StructWarning(..))
import qualified GCL.WP as POGen
import GHC.Generics (Generic)
import GHC.IO.IOMode (IOMode (ReadWriteMode))
import LSP.ExportPO ()
import Language.LSP.Diagnostics (partitionBySource)
import Language.LSP.Server
import Language.LSP.Types hiding (TextDocumentSyncClientCapabilities (..))
import Network.Simple.TCP (HostPreference (Host), serve)
import Network.Socket (socketToHandle)
import Pretty
import qualified Syntax.Abstract as A
import Syntax.Concrete (ToAbstract (toAbstract))
import Syntax.Parser
import Syntax.Predicate
  ( Origin (..),
    PO (..),
    Spec,
  )
import Data.Map (Map)
import qualified Data.Map as Map

--------------------------------------------------------------------------------

data Env = Env
  { envChan :: Chan Text,
    envDevMode :: Bool,
    -- | We maintain our own Uri-Source mapping 
    --   instead of using the built-in LSP VFS 
    --   to have better control of update management
    envSourceMap :: IORef (Map FilePath (Text, Maybe (Int, Int))),
    -- | Counter for generating fresh numbers
    envCounter :: IORef Int
  }

initEnv :: Bool -> IO Env
initEnv devMode = Env <$> newChan <*> pure devMode <*> newIORef Map.empty <*> newIORef 0

--------------------------------------------------------------------------------

type ServerM = ReaderT Env IO

runServerM :: Env -> LanguageContextEnv () -> LspT () ServerM a -> IO a
runServerM env ctxEnv program = runReaderT (runLspT ctxEnv program) env

writeLog :: Text -> LspT () ServerM ()
writeLog msg = do
  chan <- lift $ asks envChan
  liftIO $ writeChan chan msg

updateSource :: FilePath -> Text -> LspT () ServerM ()
updateSource filepath source = do
  ref <- lift $ asks envSourceMap
  liftIO $ modifyIORef' ref (Map.insertWith (\(src, _) (_, sel) -> (src, sel)) filepath (source, Nothing))

updateSelection :: FilePath -> (Int, Int) -> LspT () ServerM ()
updateSelection filepath selection = do
  ref <- lift $ asks envSourceMap
  liftIO $ modifyIORef' ref (Map.update (\(source, _) -> Just (source, Just selection)) filepath)

readSource :: FilePath -> LspT () ServerM (Maybe Text)
readSource filepath = do
  ref <- lift $ asks envSourceMap
  mapping <- liftIO $ readIORef ref
  return $ fst <$> Map.lookup filepath mapping

readLastSelection :: FilePath -> LspT () ServerM (Maybe (Int, Int))
readLastSelection filepath = do
  ref <- lift $ asks envSourceMap
  mapping <- liftIO $ readIORef ref
  return $ snd =<< Map.lookup filepath mapping

bumpCounter :: LspT () ServerM Int
bumpCounter = do 
  ref <- lift $ asks envCounter
  n <- liftIO $ readIORef ref
  liftIO $ writeIORef ref (succ n)
  return n

--------------------------------------------------------------------------------

-- entry point of the LSP server
run :: Bool -> IO Int
run devMode = do
  env <- initEnv devMode
  if devMode
    then do
      let port = "3000"
      _ <- forkIO (printLog env)
      serve (Host "127.0.0.1") port $ \(sock, _remoteAddr) -> do
        putStrLn $ "== connection established at " ++ port ++ " =="
        handle <- socketToHandle sock ReadWriteMode
        _ <- runServerWithHandles handle handle (serverDefn env)
        putStrLn "== dev server closed =="
    else do
      runServer (serverDefn env)
  where
    printLog :: Env -> IO ()
    printLog env = do
      result <- readChan (envChan env)
      when (envDevMode env) $ do
        Text.putStrLn result
      printLog env

    serverDefn :: Env -> ServerDefinition ()
    serverDefn env =
      ServerDefinition
        { onConfigurationChange = const $ pure $ Right (),
          doInitialize = \ctxEnv _req -> pure $ Right ctxEnv,
          staticHandlers = handlers,
          interpretHandler = \ctxEnv -> Iso (runServerM env ctxEnv) liftIO,
          options = lspOptions
        }

    lspOptions :: Options
    lspOptions =
      defaultOptions
        { textDocumentSync = Just syncOptions
        }

    -- these `TextDocumentSyncOptions` are essential for receiving notifications from the client
    syncOptions :: TextDocumentSyncOptions
    syncOptions =
      TextDocumentSyncOptions
        { _openClose = Just True, -- receive open and close notifications from the client
          _change = Nothing, -- receive change notifications from the client
          _willSave = Just False, -- receive willSave notifications from the client
          _willSaveWaitUntil = Just False, -- receive willSave notifications from the client
          _save = Just $ InR saveOptions
        }

    -- includes the document content on save, so that we don't have to read it from the disk
    saveOptions :: SaveOptions
    saveOptions = SaveOptions (Just True)

-- handlers of the LSP server
handlers :: Handlers (LspT () ServerM)
handlers =
  mconcat
    [ -- custom methods, not part of LSP
      requestHandler (SCustomMethod "guacamole") $ \req responder -> do
        let RequestMessage _ i _ params = req
        -- JSON Value => Request => Response
        response <- case JSON.fromJSON params of
          JSON.Error msg -> do
            writeLog " --> CustomMethod: CannotDecodeRequest"
            return $ CannotDecodeRequest $ show msg ++ "\n" ++ show params
          JSON.Success request@(Req filepath kind) -> do
            writeLog $ " --> Custom Reqeust: " <> pack (show request)
            
            result <- readSource filepath
            case result of
              Nothing -> pure NotLoaded
              Just source -> do
                -- send Diagnostics
                version <- case i of 
                  IdInt n -> return n
                  IdString _ -> bumpCounter
                sendDiagnostics filepath source version
                -- convert Request to Response
                kinds <- case kind of 
                  ReqInspect selStart selEnd -> do 
                    updateSelection filepath (selStart, selEnd)
                    return $ asGlobalError $ do
                      (pos, _specs, _globalProps, _warnings) <- checkEverything filepath source (Just (selStart, selEnd))
                      return [ResInspect pos]
                  ReqRefine index payload -> return $ asLocalError index $ do
                    _ <- refine payload
                    return [ResResolve index]
                  ReqSubstitute index expr _subst -> return $ asGlobalError $ do
                    A.Program _ _ defns _ _ <- parseProgram filepath source
                    let expr' = runSubstM (expand (A.Subst expr _subst)) defns 1
                    return [ResSubstitute index expr']
                  ReqExportProofObligations -> return $ asGlobalError $ do
                    return [ResConsoleLog "Export"]
                  ReqDebug -> return $ error "crash!"
                return $ Res filepath kinds

        writeLog $ " <-- " <> pack (show response)
        responder $ Right $ JSON.toJSON response,
      -- when the client saved the document, store the text for later use
      notificationHandler STextDocumentDidSave $ \ntf -> do
        writeLog " --> TextDocumentDidSave"
        let NotificationMessage _ _ (DidSaveTextDocumentParams (TextDocumentIdentifier uri) text) = ntf
        case text of
          Nothing -> pure ()
          Just source ->
            case uriToFilePath uri of
              Nothing -> pure ()
              Just filepath -> do
                updateSource filepath source
                version <- bumpCounter
                checkAndSendResult filepath source version
                sendDiagnostics filepath source version,
      -- when the client opened the document
      notificationHandler STextDocumentDidOpen $ \ntf -> do
        writeLog " --> TextDocumentDidOpen"
        let NotificationMessage _ _ (DidOpenTextDocumentParams (TextDocumentItem uri _ _ source)) = ntf
        case uriToFilePath uri of
          Nothing -> pure ()
          Just filepath -> do
            updateSource filepath source
            version <- bumpCounter
            checkAndSendResult filepath source version
            sendDiagnostics filepath source version
    ]

checkAndSendResult :: FilePath -> Text -> Int -> LspT () ServerM ()
checkAndSendResult filepath source version = do 
  lastSelection <- readLastSelection filepath
  let result = runM (checkEverything filepath source lastSelection)
  let response = case result of 
        Left e -> Res filepath [ResError [globalError e]]
        Right (pos, specs, globalProps, warnings) -> Res filepath [ResOK (IdInt version) pos specs globalProps warnings]
  sendNotification (SCustomMethod "guacamole") $ JSON.toJSON response

sendDiagnostics :: FilePath -> Text -> Int -> LspT () ServerM ()
sendDiagnostics filepath source version = do
  -- send diagnostics
  diags <- do
    let result = runM $ checkEverything filepath source Nothing
    return $ case result of
      Left err -> errorToDiagnostics err
      Right res -> resultToDiagnostics res
  let fileUri = toNormalizedUri (filePathToUri filepath)

  publishDiagnostics 100 fileUri (Just version) (partitionBySource diags)
  where
    resultToDiagnostics :: Result -> [Diagnostic]
    resultToDiagnostics (pos, _, _, warnings) = map proofObligationToDiagnostic pos ++ concatMap warningToDiagnostics warnings

    warningToDiagnostics :: StructWarning -> [Diagnostic]
    warningToDiagnostics (MissingBound loc) = [makeWarning loc "Bound Missing" "Bound missing at the end of the assertion before the DO construct \" , bnd : ... }\""]
    warningToDiagnostics (ExcessBound loc) = [makeWarning loc "Excess Bound" "Unnecessary bound annotation at this assertion"]

    errorToDiagnostics :: Error -> [Diagnostic]
    errorToDiagnostics (LexicalError pos) = [makeError (Loc pos pos) "Lexical error" ""]
    errorToDiagnostics (SyntacticError errs) = map syntacticErrorToDiagnostics errs
      where
        syntacticErrorToDiagnostics (loc, msg) = makeError loc "Syntax error" (Text.pack msg)
    errorToDiagnostics (StructError err) = structErrorToDiagnostics err
      where
        structErrorToDiagnostics (MissingAssertion loc) = [makeError loc "Assertion Missing" "Assertion before the DO construct is missing"]
        structErrorToDiagnostics (MissingPostcondition loc) = [makeError loc "Postcondition Missing" "The last statement of the program should be an assertion"]
        structErrorToDiagnostics (DigHole _) = []
    errorToDiagnostics (TypeError err) = typeErrorToDiagnostics err
      where
        typeErrorToDiagnostics (NotInScope name loc) = [makeError loc "Not in scope" $ "The definition " <> LazyText.toStrict name <> " is not in scope"]
        typeErrorToDiagnostics (UnifyFailed s t loc) =
          [ makeError loc "Cannot unify types" $
              renderStrict $
                "Cannot unify:" <+> pretty s <> line
                  <> "with        :" <+> pretty t
          ]
        typeErrorToDiagnostics (RecursiveType var t loc) =
          [ makeError loc "Recursive type variable" $
              renderStrict $
                "Recursive type variable:" <+> pretty var <> line
                  <> "in type             :" <+> pretty t
          ]
        typeErrorToDiagnostics (NotFunction t loc) =
          [ makeError loc "Not a function" $
              renderStrict $
                "The type" <+> pretty t <+> "is not a function type"
          ]
    errorToDiagnostics _ = []

    proofObligationToDiagnostic :: PO -> Diagnostic
    proofObligationToDiagnostic (PO _i _pre _post origin) = makeWarning loc title ""
      where
        -- we only mark the opening tokens ("do" and "if") for loops & conditionals
        first2Char :: Loc -> Loc
        first2Char NoLoc = NoLoc
        first2Char (Loc start _) = Loc start (translate 1 start)

        loc :: Loc
        loc = case origin of
          -- we only mark the closing tokens ("od" and "fi") for loops & conditionals
          AtLoop l -> first2Char l
          AtTermination l -> first2Char l
          AtIf l -> first2Char l
          others -> locOf others

        title :: Text.Text
        title = case origin of
          AtAbort {} -> "Abort"
          AtSpec {} -> "Spec"
          AtAssignment {} -> "Assignment"
          AtAssertion {} -> "Assertion"
          AtIf {} -> "Conditional"
          AtLoop {} -> "Loop Invariant"
          AtTermination {} -> "Loop Termination"
          AtSkip {} -> "Skip"

    -- translate a Pos along the same line
    translate :: Int -> Pos -> Pos
    translate n (Pos path ln col offset) = Pos path ln ((col + n) `max` 0) ((offset + n) `max` 0)

    posToPosition :: Pos -> Position
    posToPosition (Pos _path ln col _offset) = Position ((ln - 1) `max` 0) ((col - 1) `max` 0)

    locToRange :: Loc -> Range
    locToRange NoLoc = Range (Position 0 0) (Position 0 0)
    locToRange (Loc start end) = Range (posToPosition start) (posToPosition (translate 1 end))

    locToLocation :: Loc -> Location
    locToLocation NoLoc = Location (Uri "") (locToRange NoLoc)
    locToLocation (Loc start end) = Location (Uri $ Text.pack $ posFile start) (locToRange (Loc start end))

    severityToDiagnostic :: Maybe DiagnosticSeverity -> Loc -> Text.Text -> Text.Text -> Diagnostic
    severityToDiagnostic severity loc title body = Diagnostic (locToRange loc) severity Nothing Nothing title Nothing (Just $ List [DiagnosticRelatedInformation (locToLocation loc) body])

    makeWarning :: Loc -> Text.Text -> Text.Text -> Diagnostic
    makeWarning = severityToDiagnostic (Just DsWarning)

    makeError :: Loc -> Text.Text -> Text.Text -> Diagnostic
    makeError = severityToDiagnostic (Just DsError)

--------------------------------------------------------------------------------

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

type ID = LspId ('CustomMethod :: Method 'FromClient 'Request)

--------------------------------------------------------------------------------

type M = Except Error

runM :: M a -> Either Error a
runM = runExcept

ignoreError :: M [ResKind] -> [ResKind]
ignoreError program =
  case runM program of
    Left _err -> []
    Right val -> val

-- catches Error and convert it into a global ResError
asGlobalError :: M [ResKind] -> [ResKind]
asGlobalError program =
  case runM program of
    Left err -> [ResError [globalError err]]
    Right val -> val

-- catches Error and convert it into a local ResError with Hole id
asLocalError :: Int -> M [ResKind] -> [ResKind]
asLocalError i program =
  case runM program of
    Left err -> [ResError [localError i err]]
    Right val -> val

--------------------------------------------------------------------------------

-- | Parse with a parser
parse :: Parser a -> FilePath -> Text -> M a
parse p filepath = withExcept SyntacticError . liftEither . runParse p filepath . LazyText.fromStrict

-- | Parse the whole program
parseProgram :: FilePath -> Text -> M A.Program
parseProgram filepath source = toAbstract <$> parse pProgram filepath source

-- | Try to parse a piece of text in a Spec
refine :: Text -> M ()
refine = void . parse pStmts "<specification>"

type Result = ([PO], [Spec], [A.Expr], [StructWarning])

-- | Type check + generate POs and Specs
checkEverything :: FilePath -> Text -> Maybe (Int, Int) -> M Result
checkEverything filepath source mouseSelection = do
  program@(A.Program _ globalProps _ _ _) <- parseProgram filepath source
  typeCheck program
  (pos, specs, warings) <- genPO program
  case mouseSelection of 
    Nothing -> return (pos, specs, globalProps, warings)
    Just sel -> return (filterPOs sel pos, specs, globalProps, warings)

typeCheck :: A.Program -> M ()
typeCheck = withExcept TypeError . TypeChecking.checkProg

genPO :: A.Program -> M ([PO], [Spec], [StructWarning])
genPO = withExcept StructError . liftEither . POGen.sweep

--------------------------------------------------------------------------------

-- | Request
data ReqKind
  = ReqInspect Int Int
  | ReqRefine Int Text
  | ReqSubstitute Int A.Expr A.Subst
  | ReqExportProofObligations
  | ReqDebug
  deriving (Generic)

instance FromJSON ReqKind

instance Show ReqKind where
  show (ReqInspect x y) = "Inspect " <> show x <> " " <> show y
  show (ReqRefine i x) = "Refine #" <> show i <> " " <> show x
  show (ReqSubstitute i x y) = "Substitute #" <> show i <> " " <> show x <> " => " <> show y
  show ReqExportProofObligations = "ExportProofObligations"
  show ReqDebug = "Debug"

data Request = Req FilePath ReqKind
  deriving (Generic)

instance FromJSON Request

instance Show Request where
  show (Req _path kind) = show kind

--------------------------------------------------------------------------------

-- | Response
data ResKind
  = ResOK ID [PO] [Spec] [A.Expr] [StructWarning]
  | ResInspect [PO]
  | ResError [(Site, Error)]
  | ResResolve Int -- resolves some Spec
  | ResSubstitute Int A.Expr
  | ResConsoleLog Text
  deriving (Generic)

instance ToJSON ResKind

instance Show ResKind where
  show (ResOK i pos specs props warnings) =
    "OK " <> show i <> " "
      <> show (length pos)
      <> " pos, "
      <> show (length specs)
      <> " specs, "
      <> show (length props)
      <> " props, "
      <> show (length warnings)
      <> " warnings"
  show (ResInspect pos) = "Inspect " <> show (length pos) <> " POs"
  show (ResError errors) = "Error " <> show (length errors) <> " errors"
  show (ResResolve i) = "Resolve " <> show i
  show (ResSubstitute i _) = "Substitute " <> show i
  show (ResConsoleLog x) = "ConsoleLog " <> show x

data Response
  = Res FilePath [ResKind]
  | CannotDecodeRequest String
  | NotLoaded
  deriving (Generic)

instance ToJSON Response

instance Show Response where
  show (Res _path kinds) = show kinds
  show (CannotDecodeRequest s) = "CannotDecodeRequest " <> s
  show NotLoaded = "NotLoaded"

--------------------------------------------------------------------------------

-- | Instances of ToJSON
instance ToJSON Origin

instance ToJSON PO

instance ToJSON Spec
