{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module REPL where

import qualified Data.ByteString.Lazy.Char8 as BS
import qualified Data.ByteString.Char8 as Strict

import qualified Syntax.Abstract as A
import Data.Aeson
import Data.Loc
import GHC.Generics
import System.IO
import GCL


recv :: FromJSON a => IO (Maybe a)
recv = decode . BS.fromStrict <$> Strict.getLine


send :: ToJSON a => a -> IO ()
send payload = do
  Strict.putStrLn $ BS.toStrict $ encode $ payload
  hFlush stdout

--------------------------------------------------------------------------------
-- | Request

data Response
  = ProofObligations [Obligation]
  | JSONError
  | SyntaxError A.SyntaxError
  | ParseError [(Pos, String)]
  deriving (Generic)

instance ToJSON Response where

--------------------------------------------------------------------------------
-- | Response


data Request = Load FilePath | Quit
  deriving (Generic)

instance FromJSON Request where
instance ToJSON Request where

--------------------------------------------------------------------------------
-- | Instances of ToJSON

instance ToJSON Obligation where
