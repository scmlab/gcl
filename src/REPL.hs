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

-- catches Error and convert it into a global ResError
global :: REPLM Response -> IO Response
global program = do
  result <- runREPLM program
  case result of
    Left err -> return $ ResError [globalError err]
    Right val -> return val

-- catches Error and convert it into a local ResError with Hole id
local :: Int -> REPLM Response -> IO Response
local i program = do
  result <- runREPLM program
  case result of
    Left err -> return $ ResError [localError i err]
    Right val -> return val

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

--------------------------------------------------------------------------------

type REPLM = ExceptT Error IO

runREPLM :: REPLM a -> IO (Either Error a)
runREPLM = runExceptT

--------------------------------------------------------------------------------

readProgram :: FilePath -> REPLM Concrete.Program
readProgram filepath = do
  result <- liftIO $ try $ Text.readFile filepath :: REPLM (Either IOException Text)
  case result of
    Left _ -> throwError $ CannotReadFile filepath
    Right raw -> do
      tokens <- scan filepath raw
      parseProgram filepath tokens

refine :: Text -> REPLM ()
refine payload = do
  _ <- scan "<spec>" payload >>= parseSpec
  return ()

--------------------------------------------------------------------------------

scan :: FilePath -> Text -> REPLM TokStream
scan filepath = withExceptT LexicalError . liftEither . Lexer.scan filepath

parse :: Parser.Parser a -> FilePath -> TokStream -> REPLM a
parse parser filepath =
  withExceptT SyntacticError . liftEither . Parser.parse parser filepath

parseProgram :: FilePath -> TokStream -> REPLM Concrete.Program
parseProgram = parse Parser.program

parseSpec :: TokStream -> REPLM [Concrete.Stmt]
parseSpec = parse Parser.specContent "<specification>"

sweep :: Concrete.Program -> REPLM ([PO], [Spec])
sweep (Concrete.Program _ _ ds statements _) = do
  ((_, pos), specs) <-
    withExceptT StructError $
      liftEither $
        runWP (structProg statements) ds
  return (pos, specs)

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
