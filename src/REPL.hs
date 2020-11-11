{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE OverloadedStrings #-}

module REPL where

-- import Control.Monad.Writer hiding (guard)

import Control.Exception
  ( IOException,
    try,
  )
import Control.Monad.Except hiding (guard)
import Control.Monad.State hiding (guard)
import Data.Aeson hiding (Error)
import Data.Text.Lazy (Text)
import qualified Data.Text.Lazy.IO as Text
import Error
import GCL.Expr
  ( expand,
    runSubstM,
  )
import GCL.WP
  ( runWP,
    structProg,
  )
import GHC.Generics (Generic)
import Language.LSP.Types
  ( From (FromClient),
    LspId,
    Method (CustomMethod),
    MethodType (Request),
  )
import qualified Syntax.Concrete as Concrete
import qualified Syntax.Parser as Parser
import Syntax.Parser.Lexer (TokStream)
import qualified Syntax.Parser.Lexer as Lexer
import Syntax.Predicate
  ( Origin,
    PO,
    Spec,
  )

type ID = LspId ( 'CustomMethod :: Method 'FromClient 'Request)

--------------------------------------------------------------------------------

-- | The REPL Monad

-- State
data REPLState = REPLState
  { replProgram :: Maybe Concrete.Program
  }

initREPLState :: REPLState
initREPLState = REPLState Nothing

-- Monad
type REPLM = ExceptT Error (StateT REPLState IO)

runREPLM :: REPLM a -> IO (Either Error a)
runREPLM f = evalStateT (runExceptT f) initREPLState

--------------------------------------------------------------------------------

catchGlobalError :: REPLM Response -> REPLM Response
catchGlobalError program =
  program `catchError` (\err -> return $ ResError [globalError err])

catchLocalError :: Int -> REPLM Response -> REPLM Response
catchLocalError i program =
  program `catchError` (\err -> return $ ResError [localError i err])

handleRequest :: ID -> Request -> REPLM Response
handleRequest i (ReqLoad filepath) = catchGlobalError $ do
  (pos, specs, globalProps) <- load filepath
  return $ ResOK i pos specs globalProps
handleRequest _ (ReqRefine i payload) = catchLocalError i $ do
  _ <- refine payload
  return $ ResResolve i
handleRequest _ (ReqSubstitute i expr _subst) = catchGlobalError $ do
  Concrete.Program _ _ defns _ _ <- getProgram
  let expr' = runSubstM (expand (Concrete.Subst expr _subst)) defns 1
  return $ ResSubstitute i expr'
handleRequest _ ReqDebug = error "crash!"

load :: FilePath -> REPLM ([PO], [Spec], [Concrete.Expr])
load filepath = do
  result <-
    liftIO $ try $ Text.readFile filepath :: REPLM (Either IOException Text)
  case result of
    Left _ -> throwError $ CannotReadFile filepath
    Right raw -> do
      tokens <- scan filepath raw
      program@(Concrete.Program _ globalProps _ _ _) <-
        parseProgram
          filepath
          tokens
      persistProgram program
      (pos, specs) <- sweep program
      return (pos, specs, globalProps)

refine :: Text -> REPLM ()
refine payload = do
  _ <- scan "<spec>" payload >>= parseSpec
  return ()

--------------------------------------------------------------------------------

-- persistFilePath :: FilePath -> REPLM ()
-- persistFilePath filepath = modify $ \s -> s {replFilePath = Just filepath}

persistProgram :: Concrete.Program -> REPLM ()
persistProgram program = modify $ \s -> s {replProgram = Just program}

getProgram :: REPLM Concrete.Program
getProgram = do
  result <- gets replProgram
  case result of
    Nothing -> throwError NotLoaded
    Just p -> return p

--------------------------------------------------------------------------------

scan :: FilePath -> Text -> REPLM TokStream
scan filepath = withExceptT LexicalError . liftEither . Lexer.scan filepath

parse :: Parser.Parser a -> FilePath -> TokStream -> REPLM a
parse parser filepath =
  withExceptT SyntacticError . liftEither . Parser.parse parser filepath

parseProgram :: FilePath -> TokStream -> REPLM Concrete.Program
parseProgram = parse Parser.program

-- toStruct :: Concrete.Program -> REPLM (Maybe Predicate.Struct)
-- toStruct = withExceptT StructError2 . liftEither . runWPM . WP2.programToStruct

parseSpec :: TokStream -> REPLM [Concrete.Stmt]
parseSpec = parse Parser.specContent "<specification>"

sweep :: Concrete.Program -> REPLM ([PO], [Spec])
sweep (Concrete.Program _ _ ds statements _) = do
  ((_, pos), specs) <-
    withExceptT StructError $
      liftEither $
        runWP (structProg statements) ds
  return (pos, specs)

-- sweep2 :: Predicate.Struct -> REPLM ([PO], [Spec])
-- sweep2 struct = withExceptT StructError2 $
--   liftEither $
--     runWPM $ do
--       pos <- runPOM $ genPO struct
--       specs <- runSpecM $ genSpec struct
--       return (pos, specs)

--------------------------------------------------------------------------------

-- typeCheck :: Concrete.Program -> Either Error ()
-- typeCheck = first (\x -> [TypeError x]) . Type.runTM . Type.checkProg

-- execute :: Concrete.Program -> Either Error [Exec.Store]
-- execute program = if null errors then Right stores else Left errors
--   where
--     errors = map ExecError $ lefts results
--     (results, stores) = unzip $ Exec.runExNondet (Exec.execProg program) Exec.prelude

--------------------------------------------------------------------------------

-- | Request
data Request
  = ReqLoad FilePath
  | ReqRefine Int Text
  | ReqSubstitute Int Concrete.Expr Concrete.Subst
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

-- instance FromJSON Loc where
-- instance Generic Loc where
-- instance GFromZero Zero (Rep Loc) where
