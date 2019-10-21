module Main where

import Syntax.Parser

import Prelude hiding (readFile)
import Data.Text.IO (readFile)
import Text.Megaparsec.Error (errorBundlePretty)

main :: IO ()
main = do
  let filepath = "examples/a.gcl"
  raw <- readFile filepath
  case parseProgram filepath raw of
    Right syntax -> print syntax
    Left err -> putStrLn $ errorBundlePretty err
