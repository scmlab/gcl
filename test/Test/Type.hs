{-# LANGUAGE OverloadedStrings #-}

module Test.Type where

import Control.Monad.State hiding (guard)
import Data.Loc ( Loc(NoLoc) )
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
        [ 
            testCase "TBase 1" $ 
                mapM_ (\t -> actual t t @?= Right t) [TBase TInt, TBase TBool, TBase TChar],
            testCase "TBase 2" $
                mapM_ (\(t1, t2) -> actual t1 t2 @?= Left (UnifyFailed t1 t2 NoLoc)) [(TBase TInt, TBase TBool), (TBase TInt, TBase TChar), (TBase TBool, TBase TChar)],
            testCase "TFunc 1" $
                actual' [("x", TBase TInt), ("y", TBase TBool)] (TFunc (TBase TInt) (TBase TBool)) (TFunc (TVar "x") (TVar "y")) @?= Right (TFunc (TBase TInt) (TBase TBool)),
            testCase "TVar 1" $
                actual (TVar "x") (TVar "y") @?= Right (TVar "y"),
            testCase "TVar 2" $
                actual' [("x", TBase TInt)] (TVar "x") (TBase TInt) @?= Right (TBase TInt),
            testCase "TVar 3" $
                actual' [("x", TVar "y"), ("y", TVar "z"), ("z", TVar "w")] (TVar "x") (TVar "y") @?= Right (TVar "w")
        ]

actual :: Type -> Type -> Either TypeError Type
actual t1 t2 = runTM $ unify NoLoc t1 t2

actual' :: SubstT -> Type -> Type -> Either TypeError Type 
actual' sub t1 t2 = runTM $ do
    (theta, i) <- get
    put (sub ++ theta, i)
    unify NoLoc t1 t2
    