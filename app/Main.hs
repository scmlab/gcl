
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

-- import Server (run)
import Pretty ()
import System.Console.GetOpt
import System.Environment
import Prelude
import Server (run)

main :: IO ()
main = do
  (Options mode port, _) <- getArgs >>= parseOpts
  case mode of
    ModeHelp -> putStrLn $ usageInfo usage options
    ModeLSP -> do
      _ <- run False port
      return ()
    ModeDev -> do
      _ <- run True port
      return ()

--------------------------------------------------------------------------------

-- | Command-line arguments
data Mode = ModeLSP | ModeHelp | ModeDev deriving Show

data Options = Options
  { _mode :: Mode
  , _port :: String
  }

defaultOptions :: Options
defaultOptions = Options
  { _mode = ModeLSP
  , _port = "3000"
  }

options :: [OptDescr (Options -> Options)]
options =
  [ Option
      ['h']
      ["help"]
      (NoArg (\opts -> opts {_mode = ModeHelp}))
      "print this help message",
    Option
      ['d']
      ["dev"]
      (NoArg (\opts -> opts {_mode = ModeDev}))
      "for testing",
    Option
      []
      ["stdio"]
      (NoArg (\opts -> opts {_mode = ModeLSP}))
      "for testing",
    Option
      ['s']
      ["socket"]
      (OptArg (\case
        Nothing -> id
        Just portNumber -> \opts -> opts {_port = portNumber}) "PORT_NUMBER")
      "socket port number"
  ]

usage :: String
usage = "GCL v0.0.1 \nUsage: gcl [Options...]\n"

parseOpts :: [String] -> IO (Options, [String])
parseOpts argv = case getOpt Permute options argv of
  (o, n, []) -> return (foldl (flip id) defaultOptions o, n)
  (_, _, errs) -> ioError $ userError $ concat errs ++ usageInfo usage options
