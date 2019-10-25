{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import Syntax.Parser

import Prelude hiding (getContents, putStrLn)
import qualified Data.Text.IO as Text
import Data.Aeson
import qualified Data.ByteString as Strict
import Data.ByteString.Lazy hiding (putStrLn)
import Data.ByteString.Lazy.Char8 (putStrLn)
-- import Data.ByteString (getLine)

import Text.Megaparsec.Error (errorBundlePretty)
import GHC.Generics

-- import System.Environment

data Request = Check FilePath | Quit
  deriving (Generic)

instance FromJSON Request where

data Response = Ok | JSONError | ParseError String
  deriving (Generic)

instance ToJSON Response where

main :: IO ()
main = loop

  where
    loop :: IO ()
    loop = do
      request <- fromStrict <$> Strict.getLine

      case decode request of
        Just (Check filepath) -> do
          raw <- Text.readFile filepath
          case parseProgram filepath raw of
            Right syntax -> putStrLn $ encode $ Ok
            Left err -> putStrLn $ encode $ ParseError $ errorBundlePretty err
          loop
        Just Quit -> return ()
        _ -> putStrLn $ encode $ JSONError

      -- if request == "quit"
      --   then return ()
      --   else loop



  -- let filepath = "examples/a.gcl"
  -- raw <- readFile filepath
  -- case parseProgram filepath raw of
  --   Right syntax -> print syntax
  --   Left err -> putStrLn $ errorBundlePretty err
