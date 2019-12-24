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
import GCL.PreCond
import GCL.Type as Type
import Syntax.Parser.Lexer (TokStream)
import qualified Syntax.Parser.Lexer as Lexer
import qualified Syntax.Parser as Parser
import qualified Syntax.Concrete as Concrete
import qualified Syntax.Abstract as Abstract
import qualified Syntax.Lasagne as Lasagne

scan :: FilePath -> Text -> Either [Error] TokStream
scan filepath = first (\x -> [LexicalError x]) . Lexer.scan filepath

parseProgram :: FilePath -> TokStream -> Either [Error] Concrete.Program
parseProgram filepath = first (map SyntacticError) . Parser.parseProgram filepath

parseSpec :: TokStream -> Either [Error] [Concrete.Stmt]
parseSpec = first (map SyntacticError) . Parser.parseSpec

abstract :: Abstract.FromConcrete a b => a -> Either [Error] b
abstract = first (\x -> [ConvertError x]) . Abstract.abstract

typeCheck :: Abstract.Program -> Either [Error] ()
typeCheck = first (\x -> [TypeError x]) . Type.runTM . Type.checkProg

sweep :: Abstract.Program -> Either [Error] ([Obligation], [Specification])
sweep (Abstract.Program _ Nothing) = return ([], [])
sweep (Abstract.Program _ (Just (statements, postcondition))) =
    let ((_, obligations), specifications) = runM $ precondStmts statements postcondition
    in return (obligations, specifications)

makeLasagne :: Abstract.Program -> Either [Error] Lasagne.Program
makeLasagne (Abstract.Program _ Nothing) = Left []
makeLasagne (Abstract.Program _ (Just (stmts, post))) = Right $ Lasagne.makeLasagne stmts post


recv :: FromJSON a => IO (Maybe a)
recv = decode . BS.fromStrict <$> Strict.getLine

send :: ToJSON a => a -> IO ()
send payload = do
  Strict.putStrLn $ BS.toStrict $ encode $ payload
  hFlush stdout

--------------------------------------------------------------------------------
-- | Request

data Response
  = OK [Obligation] [Specification]
  | Error [(Site, Error)]
  | Resolve Int -- resolves some Spec
  deriving (Generic)

instance ToJSON Response where

--------------------------------------------------------------------------------
-- | Response


data Request = Load FilePath | Refine Int Text | Quit
  deriving (Generic)

instance FromJSON Request where
instance ToJSON Request where

--------------------------------------------------------------------------------
-- | Instances of ToJSON

instance ToJSON Obligation where
instance ToJSON Hardness where
instance ToJSON Specification where
