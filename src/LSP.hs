{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PolyKinds #-}

module LSP where

-- import Control.Monad.IO.Class

import Control.Exception (IOException, try)
import Control.Monad.Except hiding (guard)
import Data.Aeson (FromJSON, ToJSON)
import qualified Data.Aeson as JSON
import Data.List (sort)
import Data.Loc (Loc (..), Located (locOf), Pos (..), posCoff, posFile)
import qualified Data.Text as Text
import Data.Text.Lazy (Text, toStrict)
import qualified Data.Text.Lazy.IO as Text
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
    -- these `TextDocumentSyncOptions` are essential for receiving notifications like `STextDocumentDidChange`
    syncOptions :: TextDocumentSyncOptions
    syncOptions =
      TextDocumentSyncOptions
        { _openClose = Just True,
          _change = Nothing,
          _willSave = Just False,
          _willSaveWaitUntil = Just False,
          _save = Just $ InR $ SaveOptions $ Just False
        }

    lspOptions :: Options
    lspOptions =
      defaultOptions
        { textDocumentSync = Just syncOptions
        }

-- handlers of the LSP server
handlers :: Handlers (LspM ())
handlers =
  mconcat
    [ -- custom methods, not part of LSP
      requestHandler (SCustomMethod "guacamole") $ \req responder -> do
        let RequestMessage _ i _ params = req
        -- JSON Value => Request => Response
        response <- case JSON.fromJSON params of
          JSON.Error msg -> return $ CannotDecodeRequest msg
          JSON.Success request -> liftIO $ handleRequest i request
        -- respond with the Response
        responder $ Right $ JSON.toJSON response,
      -- when the client saved the document
      notificationHandler STextDocumentDidSave $ \ntf -> do
        let NotificationMessage _ _ (DidSaveTextDocumentParams (TextDocumentIdentifier uri) _) = ntf
        onNotification uri,
      -- when the client opened the document
      notificationHandler STextDocumentDidOpen $ \ntf -> do
        let NotificationMessage _ _ (DidOpenTextDocumentParams (TextDocumentItem uri _ _ _)) = ntf
        onNotification uri
    ]
  where
    onNotification :: Uri -> LspM () ()
    onNotification uri = case uriToFilePath uri of
      Nothing -> pure ()
      Just filepath -> do
        -- send diagnostics
        diags <- filepathToDiags filepath
        sendDiagnostics uri (Just 0) diags
        -- send responses
        response <- liftIO $ handleRequest (IdInt 0) $ Req filepath ReqLoad
        sendNotification (SCustomMethod "guacamole") $ JSON.toJSON response

    filepathToDiags :: MonadIO m => FilePath -> m [Diagnostic]
    filepathToDiags filepath = do
      reuslt <- liftIO $
        runM $ do
          program <- readProgram filepath
          sweep program
      return $ case reuslt of
        Left err -> errorToDiagnostics err
        Right (pos, _) ->
          map proofObligationToDiagnostic pos

    -- Analyze the file and send any diagnostics to the client in a
    -- "textDocument/publishDiagnostics" notification
    sendDiagnostics :: Uri -> Maybe Int -> [Diagnostic] -> LspM () ()
    sendDiagnostics uri version diags = do
      let fileUri = toNormalizedUri uri
      publishDiagnostics 100 fileUri version (partitionBySource diags)

-- TODO: refactor these functions

-- translate a Pos along the same line
translate :: Int -> Pos -> Pos
translate n (Pos path ln col offset) = Pos path ln ((col + n) `max` 0) ((offset + n) `max` 0)

posToPosition :: Pos -> Position
posToPosition (Pos _path ln col _offset) = Position ((ln - 1) `max` 0) ((col - 1) `max` 0)

locToLocation :: Loc -> Location
locToLocation NoLoc = Location (Uri "") (locToRange NoLoc)
locToLocation (Loc start end) = Location (Uri $ Text.pack $ posFile start) (locToRange (Loc start end))

locToRange :: Loc -> Range
locToRange NoLoc = Range (Position 0 0) (Position 0 0)
locToRange (Loc start end) = Range (posToPosition start) (posToPosition (translate 1 end))

severityToDiagnostic :: Maybe DiagnosticSeverity -> Loc -> Text.Text -> Text.Text -> Diagnostic
severityToDiagnostic severity loc title body = Diagnostic (locToRange loc) severity Nothing Nothing title Nothing (Just $ List [DiagnosticRelatedInformation (locToLocation loc) body])

makeWarning :: Loc -> Text.Text -> Text.Text -> Diagnostic
makeWarning = severityToDiagnostic (Just DsWarning)

makeError :: Loc -> Text.Text -> Text.Text -> Diagnostic
makeError = severityToDiagnostic (Just DsError)

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
    typeErrorToDiagnostics (NotInScope name loc) = [makeError loc "Not in scope" $ "The definition " <> toStrict name <> " is not in scope"]
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

--------------------------------------------------------------------------------

type ID = LspId ( 'CustomMethod :: Method 'FromClient 'Request)

handleRequest :: ID -> Request -> IO Response
handleRequest lspID (Req filepath kind) = do
  responses <- handle kind
  return $ Res filepath responses
  where
    handle :: ReqKind -> IO [ResKind]
    handle ReqLoad = global $ do
      program@(Concrete.Program _ globalProps _ _ _) <- readProgram filepath
      (pos, specs) <- sweep program
      return [ResOK lspID pos specs globalProps]
    handle (ReqInspect selStart selEnd) = global $ do
      program@(Concrete.Program _ globalProps _ _ _) <- readProgram filepath
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
      Concrete.Program _ _ defns _ _ <- readProgram filepath
      let expr' = runSubstM (expand (Concrete.Subst expr _subst)) defns 1
      return [ResSubstitute i expr']
    handle ReqExportProofObligations = do
      return [ResConsoleLog "Export"]
    handle ReqDebug = error "crash!"

-- catches Error and convert it into a global ResError
global :: M [ResKind] -> IO [ResKind]
global program = do
  result <- runM program
  case result of
    Left err -> return [ResError [globalError err]]
    Right val -> return val

-- catches Error and convert it into a local ResError with Hole id
local :: Int -> M [ResKind] -> IO [ResKind]
local i program = do
  result <- runM program
  case result of
    Left err -> return [ResError [localError i err]]
    Right val -> return val

--------------------------------------------------------------------------------

type M = ExceptT Error IO

runM :: M a -> IO (Either Error a)
runM = runExceptT

--------------------------------------------------------------------------------

readProgram :: FilePath -> M Concrete.Program
readProgram filepath = do
  result <- liftIO $ try $ Text.readFile filepath :: M (Either IOException Text)
  case result of
    Left _ -> throwError $ CannotReadFile filepath
    Right raw -> do
      tokens <- scan filepath raw
      parseProgram filepath tokens

refine :: Text -> M ()
refine payload = do
  _ <- scan "<spec>" payload >>= parseSpec
  return ()

--------------------------------------------------------------------------------

scan :: FilePath -> Text -> M TokStream
scan filepath = withExceptT LexicalError . liftEither . Lexer.scan filepath

parse :: Parser.Parser a -> FilePath -> TokStream -> M a
parse parser filepath =
  withExceptT SyntacticError . liftEither . Parser.parse parser filepath

parseProgram :: FilePath -> TokStream -> M Concrete.Program
parseProgram = parse Parser.program

parseSpec :: TokStream -> M [Concrete.Stmt]
parseSpec = parse Parser.specContent "<specification>"

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

data Request = Req FilePath ReqKind
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
