{-# LANGUAGE OverloadedStrings #-}

module Main where

import GCL.PreCond
import REPL
import Syntax.Parser
import Syntax.Abstract

import Control.Monad.Except (liftIO, liftEither)
import qualified Data.Text.Lazy.IO as Text
import Prelude
import System.Console.GetOpt
import System.Environment

main :: IO ()
main = do
  (opts, _) <- getArgs >>= parseOpts
  case optMode opts of
    ModeHelp -> putStrLn $ usageInfo usage options
    ModeREPL -> runREPL loop
    ModeDev -> runREPLTest $ do
      let filepath = "examples/b.gcl"

      raw <- liftIO $ do
        raw <- Text.readFile filepath

        putStrLn "=== raw ==="
        Text.putStrLn raw

        putStrLn "\n=== tokens ==="
        print $ scan "<test>" raw

        putStrLn "\n=== AST ==="
        return raw

      syntax <- liftEither $ parseProgram filepath raw
      program <- liftEither $ abstract syntax
      case program of
        Program _ Nothing -> liftIO $ putStrLn "<empty>"
        Program _ (Just (statements, postcondition)) -> liftIO $ do

          putStrLn "\n=== statements ==="
          print $ statements

          let ((_, obligations), specifications) = runM $ precondStmts statements postcondition

          putStrLn "\n=== proof obligations ==="
          print $ obligations

          putStrLn "\n=== specifications ==="
          print $ specifications

  where
    loop :: REPL ()
    loop = do
      request <- recv
      case request of
        Load filepath -> liftIO $ runREPL $ do
          raw <- liftIO $ Text.readFile filepath
          syntax <- liftEither $ parseProgram filepath raw
          program <- liftEither $ abstract syntax

          case program of
            Program _ Nothing -> send $ OK [] []
            Program _ (Just (statements, postcondition)) -> do
              let ((_, obligations), specifications) = runM $ precondStmts statements postcondition
              send $ OK obligations specifications

          loop
        Refine i payload -> liftIO $ runREPLLocal i $ do
          syntax <- liftEither $ parseSpec payload
          _program <- liftEither $ abstract syntax

          send $ Resolve i
          loop

        Quit -> return ()

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
