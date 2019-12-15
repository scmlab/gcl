{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module REPL where

import qualified Data.ByteString.Lazy.Char8 as BS
import qualified Data.ByteString.Char8 as Strict

import Control.Monad.Except
-- import Control.Monad.Reader hiding (local)
-- import qualified Control.Monad.Reader as Reader
import Data.Aeson hiding (Error)
import Data.Text.Lazy (Text)
import GHC.Generics
import System.IO
import GCL.PreCond
import Type

--------------------------------------------------------------------------------
-- | The REPL Monad

type REPL = ExceptT [Error] IO

-- runREPL ::

runREPL :: REPL a -> IO ()
runREPL program = do
  result <- runExceptT program
  case result of
    Left errors -> send $ Error $ map fromGlobalError errors
    Right _ -> return ()

runREPLLocal :: Int -> REPL a -> IO ()
runREPLLocal i program = do
  result <- runExceptT program
  case result of
    Left errors -> send $ Error $ map (fromLocalError i) errors
    Right _ -> return ()

-- print human readable error instead
runREPLTest :: REPL a -> IO ()
runREPLTest program = do
  result <- runExceptT program
  case result of
    Left errors -> mapM_ (liftIO . print) errors
    Right _ -> return ()

recv :: FromJSON a => REPL a
recv = do
  result <- liftIO (decode . BS.fromStrict <$> Strict.getLine)
  case result of
    Nothing -> throwError []
    Just value -> return value

send :: (ToJSON a, MonadIO m) => a -> m ()
send payload = liftIO $ do
  Strict.putStrLn $ BS.toStrict $ encode $ payload
  hFlush stdout

--------------------------------------------------------------------------------
-- | Request

data Response
  = OK [Obligation] [Specification]
  | Error [(Site, Error)]
  | Resolve Int -- resolves some Spec
  deriving (Generic)

instance ToJSON Response where

--------------------------------------------------------------------------------
-- | Response


data Request = Load FilePath | Refine Int Text | Quit
  deriving (Generic)

instance FromJSON Request where
instance ToJSON Request where

--------------------------------------------------------------------------------
-- | Instances of ToJSON

instance ToJSON Obligation where
instance ToJSON Hardness where
instance ToJSON Specification where
