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
import Data.Loc (Located (locOf))
import Data.Text (pack, unpack)
import qualified Data.Text as Text
import Data.Text.Lazy (Text)
import qualified Data.Text.Lazy.IO as Text
import Error
import GCL.Expr (expand, runSubstM)
import GCL.WP (runWP, structProg)
import GHC.Generics (Generic)
import Language.LSP.Server
import Language.LSP.Types hiding (TextDocumentSyncClientCapabilities (..))
import qualified Syntax.Concrete as Concrete
import qualified Syntax.Parser as Parser
import Syntax.Parser.Lexer (TokStream)
import qualified Syntax.Parser.Lexer as Lexer
import Syntax.Predicate
  ( Origin,
    PO,
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
    [ requestHandler (SCustomMethod "guacamole") $ \req responder -> do
        let RequestMessage _ i _ params = req
        -- JSON Value => Request => Response
        response <- case JSON.fromJSON params of
          JSON.Error msg -> return $ ResError [globalError (CannotDecodeRequest msg)]
          JSON.Success request -> liftIO $ handleRequest i request
        -- respond with the Response
        responder $ Right $ JSON.toJSON response,
      notificationHandler STextDocumentDidSave $ \ntf -> do
        let NotificationMessage _ _ (DidSaveTextDocumentParams uri _) = ntf

        case fromTextDocumentIdentifier uri of
          Nothing -> pure ()
          Just path -> do
            reuslt <- liftIO $
              runM $ do
                program <- readProgram path
                (pos, _) <- sweep program
                return pos
            case reuslt of
              Left _ -> pure ()
              Right pos -> do
                -- let locs = map locOf pos
                sendNotification SWindowShowMessage (ShowMessageParams MtWarning $ pack $ show $ JSON.toJSON ("[1, 2, 3 :: Int]" :: String))
                sendNotification (SCustomMethod "guacamole/pos") $ JSON.toJSON ("[1, 2, 3 :: Int]" :: String),
      notificationHandler STextDocumentDidOpen $ \ntf -> do
        let NotificationMessage _ _ params = ntf
        -- sendNotification SWindowShowMessage (ShowMessageParams MtWarning $ "DID OPEN!" <> pack (show $params))
        pure ()
    ]
  where
    fromTextDocumentIdentifier :: TextDocumentIdentifier -> Maybe FilePath
    fromTextDocumentIdentifier (TextDocumentIdentifier uri) =
      let (prefix, path) = Text.splitAt 7 (getUri uri)
       in if prefix == "file://" then Just (unpack path) else Nothing

--------------------------------------------------------------------------------

type ID = LspId ( 'CustomMethod :: Method 'FromClient 'Request)

handleRequest :: ID -> Request -> IO Response
handleRequest i (ReqLoad filepath) = global $ do
  program@(Concrete.Program _ globalProps _ _ _) <- readProgram filepath
  (pos, specs) <- sweep program
  return $ ResOK i pos specs globalProps
handleRequest _ (ReqRefine _ i payload) = local i $ do
  _ <- refine payload
  return $ ResResolve i
handleRequest _ (ReqSubstitute filepath i expr _subst) = global $ do
  Concrete.Program _ _ defns _ _ <- readProgram filepath
  let expr' = runSubstM (expand (Concrete.Subst expr _subst)) defns 1
  return $ ResSubstitute i expr'
handleRequest _ ReqDebug = error "crash!"

-- catches Error and convert it into a global ResError
global :: M Response -> IO Response
global program = do
  result <- runM program
  case result of
    Left err -> return $ ResError [globalError err]
    Right val -> return val

-- catches Error and convert it into a local ResError with Hole id
local :: Int -> M Response -> IO Response
local i program = do
  result <- runM program
  case result of
    Left err -> return $ ResError [localError i err]
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
data Request
  = ReqLoad FilePath
  | ReqRefine FilePath Int Text
  | ReqSubstitute FilePath Int Concrete.Expr Concrete.Subst
  | ReqDebug
  deriving (Generic)

instance FromJSON Request

--------------------------------------------------------------------------------

-- | Response
data Response
  = ResOK ID [PO] [Spec] [Concrete.Expr]
  | ResError [(Site, Error)]
  | ResResolve Int -- resolves some Spec
  | ResSubstitute Int Concrete.Expr
  deriving (Generic)

instance ToJSON Response

--------------------------------------------------------------------------------

-- | Instances of ToJSON
instance ToJSON Origin

instance ToJSON PO

instance ToJSON Spec
