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
import Syntax.Parser.Lexer as Lexer
import Syntax.Parser as Parser
import Syntax.Concrete as Concrete
import Syntax.Abstract as Abstract
import Syntax.Lasagne as Lasagne

scan :: FilePath -> Text -> Either [Error] TokStream
scan filepath = first (\x -> [LexicalError x]) . Lexer.scan filepath

parseProgram :: FilePath -> TokStream -> Either [Error] Concrete.Program
parseProgram filepath = first (map SyntacticError) . Parser.parseProgram filepath

parseSpec :: TokStream -> Either [Error] [Concrete.Stmt]
parseSpec = first (map SyntacticError) . Parser.parseSpec

abstract :: Abstract.FromConcrete a b => a -> Either [Error] b
abstract = first (\x -> [ConvertError x]) . Abstract.abstract

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
