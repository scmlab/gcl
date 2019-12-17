{-# LANGUAGE OverloadedStrings #-}

module Main where

import GCL.PreCond
import GCL.Type
import REPL
import Syntax.Parser
import Syntax.Abstract
import Type

import qualified Data.Text.Lazy.IO as Text
import Data.Loc -- for reporting type error
import Prelude
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

      putStrLn "=== raw ==="
      Text.putStrLn raw

      putStrLn "\n=== tokens ==="
      print $ scan "<test>" raw

      putStrLn "\n=== AST ==="

      let parse = do
            syntax <- parseProgram filepath raw
            abstract syntax

      case parse of
        Right (Program _ Nothing) -> putStrLn "<empty>"
        Right (Program _ (Just (statements, postcondition))) -> do

          putStrLn "\n=== statements ==="
          print $ statements

          let ((_, obligations), specifications) = runM $ precondStmts statements postcondition

          putStrLn "\n=== proof obligations ==="
          print $ obligations

          putStrLn "\n=== specifications ==="
          print $ specifications
        Left errors -> print errors

  where
    loop :: IO ()
    loop = do
      request <- recv
      case request of
        Just (Load filepath) -> do
          raw <- Text.readFile filepath

          let parse = do 
                syntax <- parseProgram filepath raw
                program <- abstract syntax
                case program of
                  Program _ Nothing -> return $ OK [] []
                  prog@(Program _ (Just (statements, postcondition))) ->
                    case runTM (checkProg prog) of
                      Right () -> do
                       let ((_, obligations), specifications) = runM $  precondStmts statements postcondition
                       return $ OK obligations specifications
                      Left terr -> Left [TypeError (locOf terr) (show terr)]

          case parse of
            Left errors -> send $ Error $ map fromGlobalError errors
            Right response -> send response

          loop
        Just (Refine i payload) -> do

          let parse = parseSpec payload >>= abstract
          case parse of
            Left errors -> send $ Error $ map (fromLocalError i) errors
            Right _ -> send $ Resolve i


          loop

        Just Quit -> return ()
        Nothing -> return ()


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
