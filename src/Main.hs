{-# LANGUAGE OverloadedStrings #-}

module Main where

import Syntax.Parser
import REPL

import Prelude
import qualified Data.Text.IO as Text

import Text.Megaparsec.Error (bundlePosState, errorBundlePretty)

import System.Console.GetOpt
import System.Environment

main :: IO ()
main = do
  (opts, _) <- getArgs >>= parseOpts
  case optMode opts of
    ModeHelp -> putStrLn $ usageInfo usage options
    ModeREPL -> loop
    ModeDev -> do
      let filepath = "examples/a.gcl"
      raw <- Text.readFile filepath
      case parseProgram filepath raw of
        Right syntax -> print syntax
        Left err -> putStrLn $ errorBundlePretty err


  where
    loop :: IO ()
    loop = do
      request <- recv
      case request of
        Just (Load filepath) -> do
          raw <- Text.readFile filepath
          case parseProgram filepath raw of
            Right _syntax -> send Ok
            Left err -> send $ ParseError (toPos $ bundlePosState err) (errorBundlePretty err)
          loop
        Just Quit -> return ()
        _ -> do
          send $ Load "filepath"
          loop

      -- if request == "quit"
      --   then return ()
      --   else loop



  -- let filepath = "examples/a.gcl"
  -- raw <- readFile filepath
  -- case parseProgram filepath raw of
  --   Right syntax -> print syntax
  --   Left err -> putStrLn $ errorBundlePretty err

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
