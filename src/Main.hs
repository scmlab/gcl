{-# LANGUAGE OverloadedStrings #-}

module Main where

import Syntax.Parser
import Syntax.Parser.TokenStream
import Syntax.Abstract
import REPL
import GCL.PreCond

import Prelude
import qualified Data.Text.IO as Text
import Data.Loc (Pos(..))

import Text.Megaparsec.Error

import System.Console.GetOpt
import System.Environment
import Text.Megaparsec (PosState)
import Text.Megaparsec.Stream (Stream(..))

collectParseErrors :: Stream s
                   => ParseErrorBundle s e
                   -> [(Pos, ParseError s e)]
collectParseErrors (ParseErrorBundle errors posState)
  = snd $ foldr f (posState, []) errors
  where
    f :: Stream s
      => ParseError s e
      -> (PosState s, [(Pos, ParseError s e)])
      -> (PosState s, [(Pos, ParseError s e)])
    f err (initial, accum) =
        let (_, next) = reachOffset (errorOffset err) initial
        in (next, (toPos next, err):accum)

main :: IO ()
main = do
  (opts, _) <- getArgs >>= parseOpts
  case optMode opts of
    ModeHelp -> putStrLn $ usageInfo usage options
    ModeREPL -> loop
    ModeDev -> do
      testLexing
      -- testParsing
      -- let filepath = "examples/b.gcl"
      -- raw <- Text.readFile filepath
      -- case parseProgram filepath raw of
      --   Right syntax -> case abstract syntax of
      --     Left err -> send $ SyntaxError err
      --     Right (Program _ Nothing) -> send $ OK [] []
      --     Right (Program _ (Just (statements, postcondition))) -> do
      --       let ((_, _obligations), specifications) = runM $ precondStmts statements postcondition
      --       print $ specifications
      --       -- send $ OK obligations specifications
      --   Left err -> do
      --     putStrLn $ errorBundlePretty err
      --     let pairs = map (\(p, e) -> (p, parseErrorTextPretty e)) $ collectParseErrors err
      --     send $ ParseError pairs

      where
        testLexing :: IO ()
        testLexing = do
          let filepath = "examples/b.gcl"
          raw <- Text.readFile filepath
          putStrLn "=== raw ==="
          Text.putStrLn raw
          putStrLn "=== processed ==="
          let lexemes = testStream $ scan "<test>" raw
          putStrLn lexemes

          -- case parseProgram filepath raw of
          --   Right syntax -> print syntax
          --   Left err -> putStrLn $ errorBundlePretty err
        testParsing :: IO ()
        testParsing = do
          let filepath = "examples/b.gcl"
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
            Right syntax -> case abstract syntax of
              Left err -> send $ SyntaxError err
              Right (Program _ Nothing) -> send $ OK [] []
              Right (Program _ (Just (statements, postcondition))) -> do
                let ((_, obligations), specifications) = runM $ precondStmts statements postcondition
                send $ OK obligations specifications
            Left err -> do
              let pairs = map (\(p, e) -> (p, parseErrorTextPretty e)) $ collectParseErrors err
              send $ ParseError pairs
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
