{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module REPL where

import qualified Data.ByteString.Lazy.Char8 as BS
import qualified Data.ByteString.Char8 as Strict
import Data.Aeson
import GHC.Generics
import System.IO

data Request = Check FilePath | Quit
  deriving (Generic)

instance FromJSON Request where

data Response = Hi | Ok | JSONError | ParseError String
  deriving (Generic)

instance ToJSON Response where

recv :: FromJSON a => IO (Maybe a)
recv = decode . BS.fromStrict <$> Strict.getLine


send :: ToJSON a => a -> IO ()
send payload = do
  Strict.putStrLn $ BS.toStrict $ encode $ payload
  hFlush stdout
