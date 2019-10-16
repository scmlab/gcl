module Main where

import Syntax.Parser

main :: IO ()
main = do
  let filepath = "examples/a.gcl"
  raw <- readFile filepath
  print $ parseProgram filepath raw
