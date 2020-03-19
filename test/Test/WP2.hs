{-# LANGUAGE OverloadedStrings #-}

module Test.WP2 where

import Test.Tasty

import qualified Test.WP2.PO as PO
import qualified Test.WP2.Struct as Struct

tests :: TestTree
tests = testGroup "WP2"
  [
    PO.tests
  , Struct.tests
  ]
