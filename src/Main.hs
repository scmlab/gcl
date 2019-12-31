{-# LANGUAGE OverloadedStrings #-}

module Main where

import REPL

import Error
import GCL.PreCond2

import Control.Monad (when)
import Data.Text.Prettyprint.Doc
import qualified Data.Text.Lazy.IO as Text
import Prelude
import Pretty ()
import System.Console.GetOpt
import System.Environment

main :: IO ()
main = do
  (opts, _) <- getArgs >>= parseOpts
  case optMode opts of
    ModeHelp -> putStrLn $ usageInfo usage options
    ModeREPL -> loop
    ModeDev -> do
      let filepath = "examples/b.gcl"

      raw <- Text.readFile filepath

      let parse = do
            tokens <- scan filepath raw
            syntax <- parseProgram filepath tokens
            program <- abstract syntax
            lasagna <- makeLasagne program
            -- typeCheck program
            (obligations, specifications) <- sweep program
            return (tokens, program, lasagna, obligations, specifications)

      case parse of
        Right (tokens, program, lasagna, obligations, specifications) -> do

          putStrLn "=== raw ==="
          Text.putStrLn raw

          putStrLn "\n=== tokens ==="
          print tokens

          putStrLn "\n=== AST ==="
          print program

          putStrLn "\n=== Lasagne ==="
          print $ pretty lasagna

          putStrLn "\n=== proof obligations (Lasagne) ==="
          mapM_ (print . pretty) (getPOs lasagna)

          putStrLn "\n=== proof obligations ==="
          mapM_ (print . pretty) obligations

          putStrLn "\n=== specifications (Lasagne) ==="
          mapM_ (print . pretty) (getSpecs lasagna)

          putStrLn "\n=== specifications ==="
          mapM_ (print . pretty) specifications

        Left errors -> print errors

  where
    loop :: IO ()
    loop = do
      request <- recv
      case request of
        Just req -> do
          keepGoing <- handleRequest req
          when keepGoing loop
        Nothing -> return ()

handleRequest :: Request -> IO Bool
handleRequest (Load filepath) = do
  raw <- Text.readFile filepath

  let parse = do
        tokens <- scan filepath raw
        syntax <- parseProgram filepath tokens
        program <- abstract syntax
        typeCheck program
        sweep program

  case parse of
    Left errors -> send $ Error $ map fromGlobalError errors
    Right (obligations, specifications) -> send $ OK obligations specifications

  return True

handleRequest (Refine i payload) = do

  let parse = scan "<spec>" payload >>= parseSpec >>= abstract
  case parse of
    Left errors -> send $ Error $ map (fromLocalError i) errors
    Right _ -> send $ Resolve i

  return True

handleRequest Quit = do
  return False

--------------------------------------------------------------------------------
-- | Command-line arguments

data Mode = ModeREPL | ModeHelp | ModeDev

data Options = Options
  { optMode :: Mode
  }

defaultOptions :: Options
defaultOptions = Options
  { optMode = ModeREPL
  }

options :: [OptDescr (Options -> Options)]
options =
  [ Option ['h']  ["help"]  (NoArg (\opts -> opts { optMode = ModeHelp }))  "print this help message"
  , Option ['d']  ["dev"]  (NoArg (\opts -> opts { optMode = ModeDev }))   "for testing"
  ]

usage :: String
usage =  "GCL v0.0.1 \nUsage: gcl [Options...]\n"

parseOpts :: [String] -> IO (Options, [String])
parseOpts argv =
  case getOpt Permute options argv of
    (o,n,[]  ) -> return (foldl (flip id) defaultOptions o, n)
    (_,_,errs) -> ioError $ userError $ concat errs ++ usageInfo usage options
