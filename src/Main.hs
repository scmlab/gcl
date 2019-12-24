{-# LANGUAGE OverloadedStrings #-}

module Main where

import GCL.PreCond
import GCL.Type
import REPL

import Syntax.Abstract (Program(..))
import GCL.PreCond2
import Error

import qualified Data.Text.Lazy.IO as Text
import Prelude
import System.Console.GetOpt
import System.Environment
import Data.Text.Prettyprint.Doc
import Pretty ()

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
            scan filepath raw
              >>= parseProgram filepath
              >>= abstract
              -- >>= makeLasagne
            -- syntax <- parseProgram filepath tokens
            --  <- abstract syntax

      case parse of
        Right (Program _ Nothing) -> putStrLn "<empty>"
        Right syntax@(Program _ (Just (statements, postcondition))) -> do

          case makeLasagne syntax of
            Right program -> do

              putStrLn "\n=== program ==="
              print $ pretty program

              let obligations = runPOM $ sweepPOs program

              putStrLn "\n=== proof obligations (Lasagne) ==="
              mapM_ (print . pretty) obligations

            Left errors -> print errors


          let ((_, obligations), specifications) = runM $ precondStmts statements postcondition

          putStrLn "\n=== proof obligations ==="
          mapM_ (print . pretty) obligations

          putStrLn "\n=== specifications ==="
          mapM_ print specifications
          --
          -- case runTM (checkProg syntax) of
          --   Right () -> do
          --     let ((_, obligations), specifications) = runM $ precondStmts statements postcondition
          --
          --     putStrLn "\n=== proof obligations ==="
          --     mapM_ print obligations
          --
          --     putStrLn "\n=== specifications ==="
          --     mapM_ print specifications
          --
          --   Left err -> do
          --     putStrLn "\n=== type error ==="
          --     print err

        Left errors -> print errors
      -- case parse of
      --   Right (Program _ Nothing) -> putStrLn "<empty>"
      --   Right prog@(Program _ (Just (statements, postcondition))) -> do
      --
      --     putStrLn "\n=== statements ==="
      --     mapM_ print statements
      --
      --     case runTM (checkProg prog) of
      --       Right () -> do
      --         let ((_, obligations), specifications) = runM $ precondStmts statements postcondition
      --
      --         putStrLn "\n=== proof obligations ==="
      --         mapM_ print obligations
      --
      --         putStrLn "\n=== specifications ==="
      --         mapM_ print specifications
      --
      --       Left err -> do
      --         putStrLn "\n=== type error ==="
      --         print err
      --
      --   Left errors -> print errors

  where
    loop :: IO ()
    loop = do
      request <- recv
      case request of
        Just (Load filepath) -> do
          raw <- Text.readFile filepath

          let parse = do
                tokens <- scan filepath raw
                syntax <- parseProgram filepath tokens
                program <- abstract syntax
                case program of
                  Program _ Nothing -> return $ OK [] []
                  prog@(Program _ (Just (statements, postcondition))) ->
                    case runTM (checkProg prog) of
                      Right () -> do
                       let ((_, obligations), specifications) = runM $  precondStmts statements postcondition
                       return $ OK obligations specifications
                      Left terr -> Left [TypeError terr]

          case parse of
            Left errors -> send $ Error $ map fromGlobalError errors
            Right response -> send response

          loop
        Just (Refine i payload) -> do

          let parse = scan "<spec>" payload >>= parseSpec >>= abstract
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
