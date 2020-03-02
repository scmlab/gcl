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
type REPLM = ExceptT [Error] (StateT REPLState IO)

runREPLM :: REPLM a -> IO (Either [Error] a)
runREPLM f = evalStateT (runExceptT f) initREPLState

--------------------------------------------------------------------------------

load :: FilePath -> REPLM ()
load filepath = do
  -- persists FilePath
  modify $ \s -> s { replFilePath = Just filepath }

  result <- liftIO $ try $ Text.readFile filepath :: REPLM (Either IOException Text)
  case result of
    Left _  -> throwError [CannotReadFile filepath]
    Right raw -> do
      tokens <- withExceptT (\x -> [LexicalError x]) $ liftEither $ Lexer.scan filepath raw
      program <- withExceptT (map SyntacticError) $ liftEither $ Parser.parse Parser.program filepath tokens
      -- persists Program
      modify $ \s -> s { replProgram = Just program }
      struct <- withExceptT (\x -> [StructError2 x]) $ liftEither $ runWPM $ WP2.programToStruct program
      -- persists Struct
      modify $ \s -> s { replStruct = Just struct }

      (pos, specs) <- withExceptT (\x -> [StructError2 x]) $ liftEither $ runWPM $ do
        pos <- runPOM $ genPO struct
        specs <- runSpecM $ genSpec struct
        return (pos, specs)

      liftIO $ send $ OK pos specs



--------------------------------------------------------------------------------

scan :: FilePath -> Text -> Either [Error] TokStream
scan filepath = first (\x -> [LexicalError x]) . Lexer.scan filepath

parse :: Parser.Parser a -> FilePath -> TokStream -> Either [Error] a
parse parser filepath = first (map SyntacticError) . Parser.parse parser filepath

parseProgram :: FilePath -> TokStream -> Either [Error] Concrete.Program
parseProgram = parse Parser.program

parseSpec :: TokStream -> Either [Error] [Concrete.Stmt]
parseSpec = parse Parser.specContent "<specification>"

-- abstract :: FromConcrete a b => a -> Either [Error] b
-- abstract = first (\x -> [ConvertError x]) . Abstract.abstract

-- typeCheck :: Concrete.Program -> Either [Error] ()
-- typeCheck = first (\x -> [TypeError x]) . Type.runTM . Type.checkProg

-- execute :: Concrete.Program -> Either [Error] [Exec.Store]
-- execute program = if null errors then Right stores else Left errors
--   where
--     errors = map ExecError $ lefts results
--     (results, stores) = unzip $ Exec.runExNondet (Exec.execProg program) Exec.prelude

-- weakestPrecondition :: [Concrete.Statement] -> Either [Error] Predicate.Pred

sweep :: Concrete.Program -> Either [Error] (Predicate.Pred, [Obligation], [Specification])
sweep (Concrete.Program _ statements _) = case runWP (wpProg statements) of
    Right ((p, obligations), specifications) -> return (p, obligations, specifications)
    Left err -> Left [StructError err]

insertAssertion :: Concrete.Program -> Int -> Either [Error] Concrete.Expr
insertAssertion program n = structError $ WP2.runWPM $ do
  let pos = case locOf program of
            Loc p _ -> linePos (posFile p) n
            NoLoc -> linePos "<untitled>" n
  struct <- WP2.programToStruct program
  case Predicate.precondAtLine n struct of
    Nothing -> throwError $ PreconditionUnknown (Loc pos pos)
    Just x -> return $ Predicate.toExpr x


structError :: Either StructError2 a -> Either [Error] a
structError f = case f of
  Right x -> return x
  Left err -> Left [StructError2 err]

sweep2 :: Concrete.Program -> Either [Error] ([PO], [Spec])
sweep2 program = structError $ WP2.sweep program

recv :: FromJSON a => IO (Maybe a)
recv = decode . BS.fromStrict <$> Strict.getLine

send :: ToJSON a => a -> IO ()
send payload = do
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
  | Insert Text
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
