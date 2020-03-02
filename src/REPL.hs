{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module REPL where

import Control.Monad.State hiding (guard)
-- import Control.Monad.Writer hiding (guard)
import Control.Monad.Except hiding (guard)

import Data.Aeson hiding (Error)
import Data.Bifunctor (first)
import qualified Data.ByteString.Lazy.Char8 as BS
import qualified Data.ByteString.Char8 as Strict
import Data.Text.Lazy (Text)
import qualified Data.Text.Lazy.IO as Text
import Data.Loc
import GHC.Generics
import System.IO
import Control.Exception (IOException, try)

import Error
import GCL.WP
import GCL.WP2
import qualified GCL.WP2 as WP2
-- import GCL.Type as Type
import Syntax.Parser.Lexer (TokStream)
import qualified Syntax.Parser.Lexer as Lexer
import qualified Syntax.Parser as Parser
import qualified Syntax.Concrete as Concrete
import qualified Syntax.Predicate as Predicate
import Syntax.Location ()
-- import qualified GCL.Exec.ExecMonad as Exec
-- import qualified GCL.Exec.ExNondet as Exec
-- import qualified GCL.Exec as Exec


--------------------------------------------------------------------------------
-- | The REPL Monad

-- State
data REPLState = REPLState
  { replFilePath :: Maybe FilePath
  , replProgram :: Maybe Concrete.Program
  , replStruct :: Maybe Predicate.Struct
  }

initREPLState :: REPLState
initREPLState = REPLState Nothing Nothing Nothing

-- Monad
type REPLM = ExceptT Error (StateT REPLState IO)

runREPLM :: REPLM a -> IO (Either Error a)
runREPLM f = evalStateT (runExceptT f) initREPLState

--------------------------------------------------------------------------------

data ReqSort = LocalReq Int | GlobalReq | BreakLoop

loop :: REPLM ()
loop = do
  request <- recv
  case classifyRequest request of
    LocalReq i -> do
      request <- handleRequest request `catchError` handleLocalError i
      send request
      loop
    GlobalReq -> do
      request <- handleRequest request `catchError` handleGlobalError
      send request
      loop
    BreakLoop -> return ()
  where
    handleGlobalError err = return $ Error [globalError err]
    handleLocalError i err = return $ Error [localError i err]

classifyRequest :: Request -> ReqSort
classifyRequest (Load _) = GlobalReq
classifyRequest (Refine i _) = LocalReq i
classifyRequest (InsertAssertion _) = GlobalReq
classifyRequest Debug = error "crash!"
classifyRequest Quit = BreakLoop

handleRequest :: Request -> REPLM Response
handleRequest (Load filepath) = do
  (pos, specs) <- load filepath
  return $ OK pos specs
handleRequest (Refine i payload) = do
  _ <- refine payload
  return $ Resolve i
handleRequest (InsertAssertion i) = do
  expr <- insertAssertion i
  return $ Insert expr
handleRequest Debug = do
  error "crash!"
handleRequest Quit =
  error "panic!"

load :: FilePath -> REPLM ([PO], [Spec])
load filepath = do
  persistFilePath filepath

  result <- liftIO $ try $ Text.readFile filepath :: REPLM (Either IOException Text)
  case result of
    Left _  -> throwError $ CannotReadFile filepath
    Right raw -> do
      tokens <- scan filepath raw
      program <- parseProgram filepath tokens
      persistProgram program
      struct <- toStruct program
      persistStruct struct

      withExceptT StructError2 $ liftEither $ runWPM $ do
        pos <- runPOM $ genPO struct
        specs <- runSpecM $ genSpec struct
        return (pos, specs)

refine :: Text -> REPLM ()
refine payload = do
  _ <- scan "<spec>" payload >>= parseSpec
  return ()

insertAssertion :: Int -> REPLM Concrete.Expr
insertAssertion n = do
  program <- getProgram
  struct <- getStruct
  withExceptT StructError2 $ liftEither $ runWPM $ do
    let pos = case locOf program of
              Loc p _ -> linePos (posFile p) n
              NoLoc -> linePos "<untitled>" n
    case Predicate.precondAtLine n struct of
      Nothing -> throwError $ PreconditionUnknown (Loc pos pos)
      Just x -> return $ Predicate.toExpr x

--------------------------------------------------------------------------------

persistFilePath :: FilePath -> REPLM ()
persistFilePath filepath = modify $ \s -> s { replFilePath = Just filepath }

persistProgram :: Concrete.Program -> REPLM ()
persistProgram program = modify $ \s -> s { replProgram = Just program }

persistStruct :: Predicate.Struct -> REPLM ()
persistStruct struct = modify $ \s -> s { replStruct = Just struct }

getProgram :: REPLM Concrete.Program
getProgram = do
  result <- gets replProgram
  case result of
    Nothing -> throwError NotLoaded
    Just p -> return p

getStruct :: REPLM Predicate.Struct
getStruct = do
  result <- gets replStruct
  case result of
    Nothing -> throwError NotLoaded
    Just p -> return p

--------------------------------------------------------------------------------

scan :: FilePath -> Text -> REPLM TokStream
scan filepath = withExceptT LexicalError . liftEither . Lexer.scan filepath

parse :: Parser.Parser a -> FilePath -> TokStream -> REPLM a
parse parser filepath = withExceptT SyntacticError . liftEither . Parser.parse parser filepath

parseProgram :: FilePath -> TokStream -> REPLM Concrete.Program
parseProgram = parse Parser.program

toStruct :: Concrete.Program -> REPLM Predicate.Struct
toStruct = withExceptT StructError2 . liftEither . runWPM . WP2.programToStruct

parseSpec :: TokStream -> REPLM [Concrete.Stmt]
parseSpec = parse Parser.specContent "<specification>"

sweep1 :: Concrete.Program -> REPLM ((Predicate.Pred, [Obligation]), [Specification])
sweep1 (Concrete.Program _ statements _) =
  withExceptT StructError $ liftEither $ runWP (wpProg statements)

--------------------------------------------------------------------------------

-- typeCheck :: Concrete.Program -> Either Error ()
-- typeCheck = first (\x -> [TypeError x]) . Type.runTM . Type.checkProg

-- execute :: Concrete.Program -> Either Error [Exec.Store]
-- execute program = if null errors then Right stores else Left errors
--   where
--     errors = map ExecError $ lefts results
--     (results, stores) = unzip $ Exec.runExNondet (Exec.execProg program) Exec.prelude

-- weakestPrecondition :: [Concrete.Statement] -> Either Error Predicate.Pred


structError :: Either StructError2 a -> Either Error a
structError f = case f of
  Right x -> return x
  Left err -> Left $ StructError2 err

sweep2 :: Concrete.Program -> Either Error ([PO], [Spec])
sweep2 program = structError $ WP2.sweep program

recv :: FromJSON a => REPLM a
recv = do
  raw <- liftIO Strict.getLine
  case decode (BS.fromStrict raw) of
    Nothing -> throwError CannotDecodeRequest
    Just x -> return x

send :: ToJSON a => a -> REPLM ()
send payload = liftIO $ do
  Strict.putStrLn $ BS.toStrict $ encode $ payload
  hFlush stdout

--------------------------------------------------------------------------------
-- | Request


data Request = Load FilePath | Refine Int Text | InsertAssertion Int | Debug | Quit
  deriving (Generic)

instance FromJSON Request where
instance ToJSON Request where

--------------------------------------------------------------------------------
-- | Response

data Response
  = OK [PO] [Spec]
  | Error [(Site, Error)]
  | Resolve Int -- resolves some Spec
  | Insert Concrete.Expr
  deriving (Generic)

instance ToJSON Response where

--------------------------------------------------------------------------------
-- | Instances of ToJSON

instance ToJSON ObliOrigin where
instance ToJSON Origin where
instance ToJSON Obligation where
instance ToJSON PO where
instance ToJSON Specification where
instance ToJSON Spec where
