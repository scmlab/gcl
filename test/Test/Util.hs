{-# LANGUAGE OverloadedStrings #-}

module Test.Util where

import Data.ByteString.Lazy (ByteString)
import qualified Data.ByteString.Lazy as B
import Test.Tasty
import Test.Tasty.HUnit

import Syntax.Parser
import Syntax.Concrete
import Data.Loc (Loc(..))
--
-- -- for testing purpose
-- parseProg :: ByteString -> IO (Program Loc)
-- parseProg src = do
--   case parseConcreteProgram "" src of
--     Left err  -> assertFailure $ show err
--     Right val -> return val
--
-- parseProc :: ByteString -> IO (Process)
-- parseProc src = do
--   case parseProcess src of
--     Left err  -> assertFailure $ show err
--     Right val -> return val
--
-- --
