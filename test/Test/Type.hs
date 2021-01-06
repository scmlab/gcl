{-# LANGUAGE OverloadedStrings #-}

module Test.Type where

import Data.Loc
import GCL.Type
import Syntax.Abstract
import Test.Tasty
import Test.Tasty.HUnit

tests :: TestTree
tests = testGroup "Type" [unifyTypes]

unifyTypes :: TestTree 
unifyTypes = 
    testGroup 
        "unify" 
        [ testCase "TBase" $ do
            let actual = runTM $ unify NoLoc (TBase TInt) (TBase TInt)
            actual @?= Right (TBase TInt)
        ]