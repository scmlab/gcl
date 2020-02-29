{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module REPL where

import Data.Aeson hiding (Error)
import Data.Bifunctor (first)
import qualified Data.ByteString.Lazy.Char8 as BS
import qualified Data.ByteString.Char8 as Strict
import Data.Text.Lazy (Text)
import GHC.Generics
import System.IO

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
-- import qualified GCL.Exec.ExecMonad as Exec
-- import qualified GCL.Exec.ExNondet as Exec
-- import qualified GCL.Exec as Exec

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

structError :: Either StructError2 a -> Either [Error] a
structError f = case f of
  Right x -> return x
  Left err -> Left [StructError2 err]

sweep2 :: Concrete.Program -> Either [Error] [PO]
sweep2 program = case WP2.sweep program of
  Right x -> return x
  Left err -> Left [StructError2 err]

recv :: FromJSON a => IO (Maybe a)
recv = decode . BS.fromStrict <$> Strict.getLine

send :: ToJSON a => a -> IO ()
send payload = do
  Strict.putStrLn $ BS.toStrict $ encode $ payload
  hFlush stdout

--------------------------------------------------------------------------------
-- | Request


data Request = Load FilePath | Refine Int Text | Debug | Quit
  deriving (Generic)

instance FromJSON Request where
instance ToJSON Request where

--------------------------------------------------------------------------------
-- | Response

data Response
  = OK [PO] [Spec]
  | Error [(Site, Error)]
  | Resolve Int -- resolves some Spec
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
