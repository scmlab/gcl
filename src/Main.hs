module Main where

import Syntax.Parser

import Prelude hiding (readFile)
import Data.Text.IO (readFile)

main :: IO ()
main = do
  let filepath = "examples/a.gcl"
  raw <- readFile filepath
  print $ parseProgram filepath raw
