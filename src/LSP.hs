{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PolyKinds #-}

module LSP where

-- import Control.Monad.IO.Class

import Control.Monad.Except hiding (guard)
import Data.Aeson (FromJSON, ToJSON)
import qualified Data.Aeson as JSON
import Data.List (sort)
import Data.Loc (Loc (..), Located (locOf), Pos (..), posCoff, posFile)
import qualified Data.Text as Text
import Data.Text (Text)
import qualified Data.Text.Lazy as TextLazy
import Error
import GCL.Expr (expand, runSubstM)
import GCL.Type (TypeError (..))
import GCL.WP (StructError (..), runWP, structProg)
import GHC.Generics (Generic)
import Language.LSP.Diagnostics (partitionBySource)
import Language.LSP.Server
import Language.LSP.Types hiding (TextDocumentSyncClientCapabilities (..))
import Pretty
import qualified Syntax.Concrete as Concrete
import qualified Syntax.Parser as Parser
import Syntax.Parser.Lexer (TokStream)
import qualified Syntax.Parser.Lexer as Lexer
import Syntax.Predicate
  ( Origin (..),
    PO (..),
    Spec,
  )

--------------------------------------------------------------------------------

-- entry point of the LSP server
run :: IO Int
run =
  runServer $
    ServerDefinition
      { onConfigurationChange = const $ pure $ Right (),
        doInitialize = \env _req -> pure $ Right env,
        staticHandlers = handlers,
        interpretHandler = \env -> Iso (runLspT env) liftIO,
        options = lspOptions
      }
  where
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
handlers :: Handlers (LspM ())
handlers =
  mconcat
    [ -- custom methods, not part of LSP
      requestHandler (SCustomMethod "guacamole") $ \req responder -> do
        let RequestMessage _ i _ params = req
        -- JSON Value => Request => Response
        response <- case JSON.fromJSON params of
          JSON.Error msg -> return $ CannotDecodeRequest $ show msg ++ "\n" ++ show params 
          JSON.Success request -> handleRequest i request 
        -- respond with the Response
        responder $ Right $ JSON.toJSON response,
      -- when the client saved the document
      notificationHandler STextDocumentDidSave $ \ntf -> do
        let NotificationMessage _ _ (DidSaveTextDocumentParams (TextDocumentIdentifier uri) text) = ntf
        case text of 
          Just source -> 
            case uriToFilePath uri of
              Nothing -> pure ()
              Just filepath -> do
                response <- handleRequest (IdInt 0) (Req filepath source ReqLoad)
                sendNotification (SCustomMethod "guacamole") $ JSON.toJSON response
          Nothing -> pure ()
        ,
      -- when the client opened the document
      notificationHandler STextDocumentDidOpen $ \ntf -> do
        let NotificationMessage _ _ (DidOpenTextDocumentParams (TextDocumentItem uri _ _ source)) = ntf
        case uriToFilePath uri of
          Nothing -> pure ()
          Just filepath -> do
            response <- handleRequest (IdInt 0) (Req filepath source ReqLoad)
            sendNotification (SCustomMethod "guacamole") $ JSON.toJSON response
    ]

handleRequest :: ID -> Request -> LspM () Response
handleRequest i request = do 
  -- convert Request to LSP side effects 
  toLSPSideEffects i request
  -- convert Request to Response
  return $ toResponse i request

--------------------------------------------------------------------------------

type ID = LspId ( 'CustomMethod :: Method 'FromClient 'Request)

toResponse :: ID -> Request -> Response
toResponse lspID (Req filepath source kind) = 
  let responses = handle kind in
  Res filepath responses
  where
    handle :: ReqKind -> [ResKind]
    handle ReqLoad = global $ do
      program@(Concrete.Program _ globalProps _ _ _) <- parseProgram filepath source
      (pos, specs) <- sweep program

      return [ResOK lspID pos specs globalProps]
    handle (ReqInspect selStart selEnd) = global $ do
      program@(Concrete.Program _ globalProps _ _ _) <- parseProgram filepath source
      (pos, specs) <- sweep program
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
    handle (ReqRefine i payload) = local i $ do
      _ <- refine payload
      return [ResResolve i]
    handle (ReqSubstitute i expr _subst) = global $ do
      Concrete.Program _ _ defns _ _ <- parseProgram filepath source
      let expr' = runSubstM (expand (Concrete.Subst expr _subst)) defns 1
      return [ResSubstitute i expr']
    handle ReqExportProofObligations = global $ do
      return [ResConsoleLog "Export"]
    handle ReqDebug = error "crash!"

toLSPSideEffects :: ID -> Request -> LspT () IO ()
toLSPSideEffects _lspID (Req filepath source kind) = handle kind
  where
    handle :: ReqKind -> LspT () IO ()
    handle ReqLoad = do
        -- send diagnostics
        diags <- do 
          let reuslt = runM $ parseProgram filepath source >>= sweep
          return $ case reuslt of
            Left err -> errorToDiagnostics err
            Right (pos, _) ->
              map proofObligationToDiagnostic pos
        let fileUri = toNormalizedUri (filePathToUri filepath)
        let version = Just 0

        publishDiagnostics 100 fileUri version (partitionBySource diags)

        -- send response
        let response = toResponse (IdInt 0) $ Req filepath source ReqLoad
        sendNotification (SCustomMethod "guacamole") $ JSON.toJSON response
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
            typeErrorToDiagnostics (NotInScope name loc) = [makeError loc "Not in scope" $ "The definition " <> TextLazy.toStrict name <> " is not in scope"]
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

    handle ReqExportProofObligations = createPOFile
      where 
        exportFilepath :: Text.Text
        exportFilepath = Text.pack filepath <> ".md"

        createPOFile :: LspT () IO () 
        createPOFile = do
          let uri = Uri exportFilepath
          let createFile = CreateFile uri Nothing
          let edit = WorkspaceEdit Nothing (Just (List [InR (InL createFile)]))
          _ <- sendRequest SWorkspaceApplyEdit (ApplyWorkspaceEditParams (Just "create export file") edit) handleCreatePOFile
          pure ()

        handleCreatePOFile :: Either ResponseError ApplyWorkspaceEditResponseBody -> LspT () IO ()
        handleCreatePOFile (Left (ResponseError _ message _)) = sendNotification SWindowShowMessage (ShowMessageParams MtError $ "Failed to export proof obligations: \n" <> message)
        handleCreatePOFile (Right (ApplyWorkspaceEditResponseBody False Nothing)) = sendNotification SWindowShowMessage (ShowMessageParams MtWarning $ exportFilepath <> " already existed")
        handleCreatePOFile (Right _) = exportPOs

        exportPOs :: LspT () IO () 
        exportPOs = do 
          let result = runM $ do 
                program <- parseProgram filepath source
                (pos, _) <- sweep program
                return pos
          case result of 
            Left err -> do 
              let message = Text.pack $ show err
              sendNotification SWindowShowMessage (ShowMessageParams MtError $ "Failed calculate proof obligations: \n" <> message)
            Right pos -> do 
              let toMarkdown (PO i pre post _) = pretty i <> "." <+> pretty pre <+> "=>" <+> pretty post 
              let content = renderStrict $ concatWith (\x y -> x <> line <> y) $ map toMarkdown pos

              let identifier = VersionedTextDocumentIdentifier (Uri exportFilepath) (Just 0)
              let range = Range (Position 0 0) (Position 0 0)
              let textEdits = [TextEdit range content]
              -- let textEdits = map (TextEdit range . renderStrict . toMarkdown) pos
              let textDocEdit = TextDocumentEdit identifier $ List textEdits
              let edit = WorkspaceEdit Nothing (Just (List [InL textDocEdit]))
              _ <- sendRequest SWorkspaceApplyEdit (ApplyWorkspaceEditParams (Just "writing proof obligations") edit) handleExportPos

              -- sendNotification SWindowShowMessage (ShowMessageParams MtInfo $ "a\nb")
              -- sendNotification SWindowLogMessage (LogMessageParams MtInfo $ "a\nb")

              pure ()

        handleExportPos :: Either ResponseError ApplyWorkspaceEditResponseBody -> LspT () IO ()
        handleExportPos (Left (ResponseError _ message _)) = sendNotification SWindowShowMessage (ShowMessageParams MtError $ "Failed to write proof obligations: \n" <> message)
        handleExportPos (Right message) = sendNotification SWindowShowMessage (ShowMessageParams MtWarning $ Text.pack $ show message)


    handle _ = pure ()

-- catches Error and convert it into a global ResError
global :: M [ResKind] -> [ResKind]
global program =
  case runM program of
    Left err -> [ResError [globalError err]]
    Right val -> val

-- catches Error and convert it into a local ResError with Hole id
local :: Int -> M [ResKind] -> [ResKind]
local i program = 
  case runM program of
    Left err -> [ResError [localError i err]]
    Right val -> val

--------------------------------------------------------------------------------

type M = Except Error

runM :: M a -> Either Error a
runM = runExcept

--------------------------------------------------------------------------------

scan :: FilePath -> Text -> M TokStream
scan filepath = withExceptT LexicalError . liftEither . Lexer.scan filepath . TextLazy.fromStrict

parse :: Parser.Parser a -> FilePath -> TokStream -> M a
parse parser filepath =
  withExceptT SyntacticError . liftEither . Parser.parse parser filepath

parseProgram :: FilePath -> Text -> M Concrete.Program
parseProgram filepath source = do
  tokens <- scan filepath source
  parse Parser.program filepath tokens

refine :: Text -> M ()
refine payload = do
  _ <- scan "<spec>" payload >>= parse Parser.specContent "<specification>"
  return ()

sweep :: Concrete.Program -> M ([PO], [Spec])
sweep (Concrete.Program _ _ ds statements _) = do
  ((_, pos), specs) <-
    withExceptT StructError $
      liftEither $
        runWP (structProg statements) ds
  return (pos, specs)

--------------------------------------------------------------------------------

-- | Request
data ReqKind
  = ReqLoad
  | ReqInspect Int Int
  | ReqRefine Int Text
  | ReqSubstitute Int Concrete.Expr Concrete.Subst
  | ReqExportProofObligations
  | ReqDebug
  deriving (Generic)

instance FromJSON ReqKind

data Request = Req FilePath Text ReqKind
  deriving (Generic)

instance FromJSON Request

--------------------------------------------------------------------------------

-- | Response
data ResKind
  = ResOK ID [PO] [Spec] [Concrete.Expr]
  | ResError [(Site, Error)]
  | ResResolve Int -- resolves some Spec
  | ResSubstitute Int Concrete.Expr
  | ResConsoleLog Text
  deriving (Generic)

instance ToJSON ResKind

data Response = Res FilePath [ResKind] | CannotDecodeRequest String
  deriving (Generic)

instance ToJSON Response

--------------------------------------------------------------------------------

-- | Instances of ToJSON
instance ToJSON Origin

instance ToJSON PO

instance ToJSON Spec
