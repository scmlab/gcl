{-# LANGUAGE OverloadedStrings #-}

module Main where

import           REPL

import           Data.Text.Prettyprint.Doc
import qualified Data.Text.Lazy.IO             as Text
import           Prelude
import           Pretty                         ( )
import           System.Console.GetOpt
import           System.Environment

main :: IO ()
main = do
  (opts, _) <- getArgs >>= parseOpts
  case optMode opts of
    ModeHelp -> putStrLn $ usageInfo usage options
    ModeREPL -> do
      _ <- runREPLM loop
      return ()
    ModeDev -> do
      let filepath = "examples/b.gcl"
      raw    <- Text.readFile filepath

      result <- runREPLM $ do
        tokens                        <- scan filepath raw
        program                       <- parseProgram filepath tokens
        -- typeCheck program
        (obligations, specifications) <- sweep1 program
        -- stores <- execute program
        return (tokens, program, obligations, specifications)

      case result of
        Right (tokens, program, obligations, specifications) -> do

          putStrLn "\n=== tokens ==="
          print tokens

          putStrLn "\n=== AST ==="
          print program

          putStrLn "\n=== proof obligations ==="
          mapM_ (print . pretty) obligations

          putStrLn "\n=== specifications ==="
          mapM_ (print . pretty) specifications

          -- putStrLn "\n=== execution (stores) ==="
          -- mapM_ (print . pretty) stores

        Left err -> do
          print $ pretty err

--------------------------------------------------------------------------------
-- | Command-line arguments

data Mode = ModeREPL | ModeHelp | ModeDev

data Options = Options
  { optMode :: Mode
  }

defaultOptions :: Options
defaultOptions = Options { optMode = ModeREPL }

options :: [OptDescr (Options -> Options)]
options =
  [ Option ['h']
           ["help"]
           (NoArg (\opts -> opts { optMode = ModeHelp }))
           "print this help message"
  , Option ['d']
           ["dev"]
           (NoArg (\opts -> opts { optMode = ModeDev }))
           "for testing"
  ]

usage :: String
usage = "GCL v0.0.1 \nUsage: gcl [Options...]\n"

parseOpts :: [String] -> IO (Options, [String])
parseOpts argv = case getOpt Permute options argv of
  (o, n, []  ) -> return (foldl (flip id) defaultOptions o, n)
  (_, _, errs) -> ioError $ userError $ concat errs ++ usageInfo usage options
