{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE TypeOperators #-}

module LSP where

-- import Control.Monad.IO.Class

import Control.Exception (IOException, try)
import Control.Monad.Except hiding (guard)
import Data.Aeson (FromJSON, ToJSON)
import qualified Data.Aeson as JSON
import Data.List (sort)
import Data.Loc (Loc (..), Located (locOf), Pos (..), posCoff, posFile)
import qualified Data.Text as Text
import Data.Text.Lazy (Text)
import qualified Data.Text.Lazy.IO as Text
import Error
import GCL.Expr (expand, runSubstM)
import GCL.WP (runWP, structProg)
import GHC.Generics (Generic)
import Language.LSP.Diagnostics (partitionBySource)
import Language.LSP.Server
import Language.LSP.Types hiding (TextDocumentSyncClientCapabilities (..))
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
      notificationHandler STextDocumentDidSave $ \ntf ->
        do
          let NotificationMessage _ _ (DidSaveTextDocumentParams (TextDocumentIdentifier uri) _) = ntf
          pos <- uriToPOs uri
          sendDiagnostics uri (Just 0) pos,
      -- when the client opened the document
      notificationHandler
        STextDocumentDidOpen
        $ \ntf -> do
          let NotificationMessage _ _ (DidOpenTextDocumentParams (TextDocumentItem uri _ _ _)) = ntf
          pos <- uriToPOs uri
          sendDiagnostics uri (Just 0) pos

          -- sendNotification SWindowShowMessage (ShowMessageParams MtWarning $ pack $ show $ JSON.toJSON ("[1, 2, 3 :: Int]" :: String))
    ]
  where
    uriToPOs :: MonadIO m => Uri -> m [PO]
    uriToPOs uri = case uriToFilePath uri of
      Nothing -> return []
      Just filepath -> do
        reuslt <- liftIO $
          runM $ do
            program <- readProgram filepath
            sweep program
        case reuslt of
          Left _ -> return []
          Right (pos, _) -> return pos

    -- Analyze the file and send any diagnostics to the client in a
    -- "textDocument/publishDiagnostics" notification
    sendDiagnostics :: Uri -> Maybe Int -> [PO] -> LspM () ()
    sendDiagnostics uri version pos = do
      let fileUri = toNormalizedUri uri
      let diags = map proofObligationToDiagnostic pos
      publishDiagnostics 100 fileUri version (partitionBySource diags)

proofObligationToDiagnostic :: PO -> Diagnostic
proofObligationToDiagnostic (PO _i _pre _post origin) = Diagnostic range severity code source message tags infos
  where
    range :: Range
    range = case origin of
      -- we only mark the closing tokens ("od" and "fi") for loops & conditionals
      AtLoop loc -> locToRange' loc
      AtTermination loc -> locToRange' loc
      AtIf loc -> locToRange' loc
      others -> locToRange (locOf others)

    severity :: Maybe DiagnosticSeverity
    severity = Just DsWarning

    code :: Maybe (Int |? String)
    code = Nothing

    source :: Maybe Text.Text
    source = Nothing

    message :: Text.Text
    message = case origin of
      AtAbort {} -> "Abort"
      AtSpec {} -> "Spec"
      AtAssignment {} -> "Assignment"
      AtAssertion {} -> "Assertion"
      AtIf {} -> "Conditional"
      AtLoop {} -> "Loop Invariant"
      AtTermination {} -> "Loop Termination"
      AtSkip {} -> "Skip"

    tags :: Maybe (List DiagnosticTag)
    tags = Nothing

    location :: Location
    location = locToLocation $ locOf origin

    infos :: Maybe (List DiagnosticRelatedInformation)
    -- infos = Nothing
    infos = Just $ List [DiagnosticRelatedInformation location ""]

    locToRange :: Loc -> Range
    locToRange NoLoc = Range (Position 0 0) (Position 0 0)
    locToRange (Loc start end) = Range (translate (-1) (posToPosition start)) (posToPosition end)

    -- translate the Position along the same line
    translate :: Int -> Position -> Position
    translate n (Position line col) = Position line ((col + n) `max` 0)

    locToRange' :: Loc -> Range
    locToRange' NoLoc = Range (Position 0 0) (Position 0 0)
    locToRange' (Loc _ end) = let pos = posToPosition end in Range (translate (-2) pos) pos

    posToPosition :: Pos -> Position
    posToPosition (Pos _path line col _offset) = Position (line - 1) col

    locToLocation :: Loc -> Location
    locToLocation NoLoc = Location (Uri "") (locToRange NoLoc)
    locToLocation (Loc start end) = Location (Uri $ Text.pack $ posFile start) (locToRange (Loc start end))

--------------------------------------------------------------------------------

type ID = LspId ( 'CustomMethod :: Method 'FromClient 'Request)

-- type ID = Int

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
    -- return [ResOK lspID pos specs globalProps, ResDecorate (map locOf pos)]
    handle (ReqInspect selStart selEnd) = global $ do
      program <- readProgram filepath
      pos <- fst <$> sweep program
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
      let overlapped = sort $ filter isOverlapped pos
      return [ResOK lspID overlapped [] []]
    -- return [ResOK lspID overlapped [] [], ResDecorate (map locOf pos)]
    handle (ReqRefine i payload) = local i $ do
      _ <- refine payload
      return [ResResolve i]
    handle (ReqSubstitute i expr _subst) = global $ do
      Concrete.Program _ _ defns _ _ <- readProgram filepath
      let expr' = runSubstM (expand (Concrete.Subst expr _subst)) defns 1
      return [ResSubstitute i expr']
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
  | ResDecorate [Loc]
  | ResResolve Int -- resolves some Spec
  | ResSubstitute Int Concrete.Expr
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
