{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PolyKinds #-}

module LSP where

-- import Control.Monad.IO.Class

import Control.Concurrent (forkIO)
import Control.Concurrent.Chan (writeChan, readChan, Chan, newChan)
import Control.Monad.Except hiding (guard)
import Data.Aeson (FromJSON, ToJSON)
import qualified Data.Aeson as JSON
import Data.List (sort)
import Data.Loc (Loc (..), Located (locOf), Pos (..), posCoff, posFile)
import Data.Text (Text, pack)
import qualified Data.Text as Text
import qualified Data.Text.IO as Text
import qualified Data.Text.Lazy as LazyText
import Error
import GCL.Expr (expand, runSubstM)
import qualified GCL.Type as TypeChecking
import GCL.Type (TypeError(..))
import qualified GCL.WP as POGen
import GCL.WP (StructError (..))
import GHC.Generics (Generic)
import GHC.IO.IOMode (IOMode (ReadWriteMode))
import Language.LSP.Diagnostics (partitionBySource)
import Language.LSP.Server
import Language.LSP.Types hiding (TextDocumentSyncClientCapabilities (..))
import Network.Simple.TCP (HostPreference (Host), serve)
import Network.Socket (socketToHandle)
import Pretty
import qualified Syntax.Abstract as A
import Syntax.Concrete (ToAbstract (toAbstract))
-- import qualified Syntax.Parser as Parser
-- import Syntax.Parser.Lexer (TokStream)
-- import qualified Syntax.Parser.Lexer as Lexer
import Syntax.Parser
import Syntax.Predicate
  ( Origin (..),
    PO (..),
    Spec,
  )
import Control.Monad.Reader
import LSP.ExportPO ()

--------------------------------------------------------------------------------


data Env = Env
  { envChan :: Chan Text
  , envDevMode :: Bool
  }

type ServerM = ReaderT Env IO

runServerM :: Env -> LanguageContextEnv () -> LspT () ServerM a -> IO a 
runServerM env ctxEnv program = runReaderT (runLspT ctxEnv program) env

writeLog :: Text -> LspT () ServerM ()
writeLog msg = do 
  chan <- lift $ asks envChan
  liftIO $ writeChan chan msg

--------------------------------------------------------------------------------

-- entry point of the LSP server
run :: Bool -> IO Int
run devMode = do
  chan <- newChan
  if devMode
    then do
      let env = Env chan True
      let port = "3000"
      _ <- forkIO (printLog env)
      serve (Host "localhost") port $ \(sock, _remoteAddr) -> do
        putStrLn $ "== connection established at " ++ port ++ " =="
        handle <- socketToHandle sock ReadWriteMode
        _ <- runServerWithHandles handle handle (serverDefn env)
        putStrLn "== dev server closed =="
    else do 
      let env = Env chan False
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
          JSON.Success request -> do 
            writeLog $ " --> Custom Reqeust: " <> pack (show request)
            handleRequest i request

        writeLog $ " <-- " <> pack (show response)
        -- respond with the Response
        responder $ Right $ JSON.toJSON response,
      -- when the client saved the document
      notificationHandler STextDocumentDidSave $ \ntf -> do
        writeLog " --> TextDocumentDidSave"
        let NotificationMessage _ _ (DidSaveTextDocumentParams (TextDocumentIdentifier uri) text) = ntf
        case text of
          Just source ->
            case uriToFilePath uri of
              Nothing -> pure ()
              Just filepath -> do
                response <- handleRequest (IdInt 0) (Req filepath source ReqLoad)
                writeLog $ " <-- " <> pack (show response)
                sendNotification (SCustomMethod "guacamole") $ JSON.toJSON response
          Nothing -> pure (),
      -- when the client opened the document
      notificationHandler STextDocumentDidOpen $ \ntf -> do
        writeLog " --> TextDocumentDidOpen"
        let NotificationMessage _ _ (DidOpenTextDocumentParams (TextDocumentItem uri _ _ source)) = ntf
        case uriToFilePath uri of
          Nothing -> pure ()
          Just filepath -> do
            response <- handleRequest (IdInt 0) (Req filepath source ReqLoad)
            writeLog $ " <-- " <> pack (show response)
            sendNotification (SCustomMethod "guacamole") $ JSON.toJSON response
    ]

handleRequest :: ID -> Request -> LspT () ServerM Response
handleRequest i request = do
  -- convert Request to LSP side effects
  toLSPSideEffects i request
  -- convert Request to Response
  return $ toResponse i request

--------------------------------------------------------------------------------

type ID = LspId ('CustomMethod :: Method 'FromClient 'Request)

toResponse :: ID -> Request -> Response
toResponse lspID (Req filepath source kind) =
  let responses = handle kind
   in Res filepath responses
  where
    handle :: ReqKind -> [ResKind]
    handle ReqLoad = asGlobalError $ do
      (pos, specs, globalProps) <- check filepath source 

      return [ResOK lspID pos specs globalProps]
    handle (ReqInspect selStart selEnd) = asGlobalError $ do
      (pos, specs, globalProps) <- check filepath source
      -- find the POs whose Range overlaps with the selection
      let isOverlapped po = case locOf po of
            NoLoc -> False
            Loc start' end' ->
              let start = posCoff start'
                  end = posCoff end' + 1
               in (selStart <= start && selEnd >= start) -- the end of the selection overlaps with the start of PO
                    || (selStart <= end && selEnd >= end) -- the start of the selection overlaps with the end of PO
                    || (selStart <= start && selEnd >= end) -- the selection covers the PO
                    || (selStart >= start && selEnd <= end) -- the selection is within the PO
                    -- sort them by comparing their starting position
      let overlapped = reverse $ sort $ filter isOverlapped pos
      let nearest = reverse $ case overlapped of
            [] -> []
            (x : _) -> case locOf x of
              NoLoc -> []
              Loc start _ ->
                let same y = case locOf y of
                      NoLoc -> False
                      Loc start' _ -> start == start'
                 in filter same overlapped
      return [ResOK lspID nearest specs globalProps]
    handle (ReqRefine i payload) = asLocalError i $ do
      _ <- refine payload
      return [ResResolve i]
    handle (ReqSubstitute i expr _subst) = asGlobalError $ do
      A.Program _ _ defns _ _ <- parseProgram filepath source
      let expr' = runSubstM (expand (A.Subst expr _subst)) defns 1
      return [ResSubstitute i expr']
    handle ReqExportProofObligations = asGlobalError $ do
      return [ResConsoleLog "Export"]
    handle ReqDebug = error "crash!"

toLSPSideEffects :: ID -> Request -> LspT () ServerM ()
toLSPSideEffects _lspID (Req filepath source kind) = handle kind
  where
    handle :: ReqKind -> LspT () ServerM ()
    handle ReqLoad = do
      -- send diagnostics
      diags <- do
        let result = runM $ check filepath source
        return $ case result of
          Left err -> errorToDiagnostics err
          Right (pos, _, _) -> map proofObligationToDiagnostic pos
      let fileUri = toNormalizedUri (filePathToUri filepath)
      let version = Just 0

      publishDiagnostics 100 fileUri version (partitionBySource diags)
      where
        errorToDiagnostics :: Error -> [Diagnostic]
        errorToDiagnostics (LexicalError pos) = [makeError (Loc pos pos) "Lexical error" ""]
        errorToDiagnostics (SyntacticError errs) = map syntacticErrorToDiagnostics errs
          where
            syntacticErrorToDiagnostics (loc, msg) = makeError loc "Syntax error" (Text.pack msg)
        errorToDiagnostics (StructError err) = structErrorToDiagnostics err
          where
            structErrorToDiagnostics (MissingAssertion loc) = [makeError loc "Assertion Missing" "Assertion before the DO construct is missing"]
            structErrorToDiagnostics (MissingBound loc) = [makeError loc "Bound Missing" "Bound missing at the end of the assertion before the DO construct \" , bnd : ... }\""]
            structErrorToDiagnostics (ExcessBound loc) = [makeError loc "Excess Bound" "Unnecessary bound annotation at this assertion"]
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
    handle ReqExportProofObligations = return ()
    handle _ = pure ()

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
parseProgram filepath source = do
  toAbstract <$> parse pProgram filepath source
  
-- | Try to parse a piece of text in a Spec
refine :: Text -> M ()
refine = void . parse pStmts "<specification>"

-- | Type check + generate POs and Specs
check :: FilePath -> Text -> M ([PO], [Spec], [A.Expr])
check filepath source = do 
  program@(A.Program _ globalProps _ _ _) <- parseProgram filepath source
  typeCheck program
  (pos, specs) <- genPO program
  return (pos, specs, globalProps)
  where 
    typeCheck :: A.Program -> M ()
    typeCheck = withExcept TypeError . TypeChecking.checkProg

    genPO :: A.Program -> M ([PO], [Spec])
    genPO = withExcept StructError . liftEither . POGen.sweep

--------------------------------------------------------------------------------

-- | Request
data ReqKind
  = ReqLoad
  | ReqInspect Int Int
  | ReqRefine Int Text
  | ReqSubstitute Int A.Expr A.Subst
  | ReqExportProofObligations
  | ReqDebug
  deriving (Generic)

instance FromJSON ReqKind

instance Show ReqKind where 
  show ReqLoad = "Load"
  show (ReqInspect x y) = "Inspect " <> show x <> " " <> show y
  show (ReqRefine i x) = "Refine #" <> show i <> " " <> show x
  show (ReqSubstitute i x y) = "Substitute #" <> show i <> " " <> show x <> " => " <> show y
  show ReqExportProofObligations = "ExportProofObligations"
  show ReqDebug = "Debug"

data Request = Req FilePath Text ReqKind
  deriving (Generic)

instance FromJSON Request

instance Show Request where 
  show (Req _path _content kind) = show kind 

--------------------------------------------------------------------------------

-- | Response
data ResKind
  = ResOK ID [PO] [Spec] [A.Expr]
  | ResError [(Site, Error)]
  | ResResolve Int -- resolves some Spec
  | ResSubstitute Int A.Expr
  | ResConsoleLog Text
  deriving (Generic)

instance ToJSON ResKind

instance Show ResKind where 
  show (ResOK i pos specs props) = "OK " <> show i <> " " 
    <> show (length pos) <> " pos, "
    <> show (length specs) <> " specs, "
    <> show (length props) <> " props"
  show (ResError errors) = "Error " <> show (length errors) <> " errors"
  show (ResResolve i) = "Resolve " <> show i
  show (ResSubstitute i _) = "Substitute " <> show i
  show (ResConsoleLog x) = "ConsoleLog " <> show x

data Response = Res FilePath [ResKind] | CannotDecodeRequest String
  deriving (Generic)

instance ToJSON Response

instance Show Response where 
  show (Res _path kinds) = show kinds 
  show (CannotDecodeRequest s) = "CannotDecodeRequest " <> s

--------------------------------------------------------------------------------

-- | Instances of ToJSON
instance ToJSON Origin

instance ToJSON PO

instance ToJSON Spec
