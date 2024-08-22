
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

-- import Server (run)
import Pretty ()
import System.Console.GetOpt
import System.Environment
import Prelude
import Server (runOnPort, runOnStdio)

main :: IO ()
main = do
  (Options mode logFilePath, _) <- getArgs >>= parseOpts
  case mode of
    ModeHelp -> putStrLn $ usageInfo usage options
    ModeRun -> do
      _ <- runOnStdio logFilePath
      return ()

--------------------------------------------------------------------------------

-- | Command-line arguments
data Mode = ModeHelp | ModeRun deriving Show

data Options = Options
  { _mode :: Mode
  , _out  :: Maybe FilePath
  }
  deriving Show

defaultOptions :: Options
defaultOptions = Options
  { _mode = ModeRun
  , _out  = Nothing
  }

options :: [OptDescr (Options -> Options)]
options =
  [ Option
      ['h']
      ["help"]
      (NoArg (\opts -> opts {_mode = ModeHelp}))
      "print this help message",
    Option
      []
      ["stdio"]
      (NoArg (\opts -> opts {_mode = ModeRun}))
      "for debugging",
    Option
      ['o']
      ["out"]
      (ReqArg (\logFilePath opts -> opts {_out = Just logFilePath}) "LOG_FILE_PATH")
      "log file path when -d is set"
  ]

usage :: String
usage = "GCL v0.0.1 \nUsage: gcl [Options...]\n"

parseOpts :: [String] -> IO (Options, [String])
parseOpts argv = case getOpt Permute options argv of
  (o, n, []) -> return (foldl (flip id) defaultOptions o, n)
  (_, _, errs) -> ioError $ userError $ concat errs ++ usageInfo usage options
