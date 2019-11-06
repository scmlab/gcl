{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DeriveGeneric #-}

module GCL.PreCond where

import Control.Monad.State hiding (guard)
import Control.Monad.Writer hiding (guard)

import qualified Data.Map as Map
-- import Data.Map (Map)
import Data.Tuple (swap)
import GHC.Generics

import Syntax.Abstract
import Syntax.Parser

data Obligation = Obligation Index Pred deriving (Show, Generic)

type M = WriterT [Obligation] (State Int)

runM :: M a -> (a, [Obligation])
runM p = evalState (runWriterT p) 0

-- creates a proof obligation
obligate :: Pred -> M ()
obligate p = do
  i <- get
  put (succ i)
  tell [Obligation i p]

conjunct :: [Pred] -> Pred
conjunct = foldr Conj (Lit False)

disjunct :: [Pred] -> Pred
disjunct = foldr Disj (Lit True)

precondStmts :: [Stmt] -> Pred -> M Pred
precondStmts [] post = return post
precondStmts (x:xs) post =
  precondStmts xs post >>= precond x

-- calculating the weakest precondition
precond :: Stmt -> Pred -> M Pred

precond Abort _ = return (Lit False)

precond Skip post = return post

precond (Assert pre) post = do
  -- generate a proof obligation,
  -- if the precondition doesn't coincides with the postcondition
  unless (predEq pre post) $ do
    obligate $ pre `Implies` post

  return pre

precond (Assign xs es) post = return $ substP (Map.fromList (zip xs es)) post

precond (If (Just pre) branches) post = do
  mapM_ (obligate <=< obliGuard pre post) branches
  let (guards, _) = unzipGdCmds branches
  obligate $ pre `Implies` disjunct guards
  return pre
  where
    obliGuard :: Pred -> Pred -> GdCmd -> M Pred
    obliGuard pre' post' (GdCmd guard body) = Implies (pre' `Conj` guard) <$> precondStmts body post'
precond (If Nothing branches) post = do
  brConds <- mapM (precondGuard post) branches
  let (guards, _) = unzipGdCmds branches

  return (conjunct brConds `Conj` disjunct guards)

precond (Do inv bnd branches) post = do

  mapM_ (obligate <=< branchCond) branches
  mapM_ (obligate <=< termCond) branches

  let (guards, _) = unzipGdCmds branches

  obligate $ (inv `Conj` (conjunct (map Neg guards)))
                  `Implies` post -- empty branches?
  obligate $ (inv `Conj` disjunct guards) `Implies` (Term GEq bnd (LitE (Num 0)))

  return inv

  where
    branchCond :: GdCmd -> M Pred
    branchCond (GdCmd guard body) = Implies (inv `Conj` guard) <$> precondStmts body inv

    termCond :: GdCmd -> M Pred
    termCond (GdCmd guard body) = do
      pre <- precondStmts body (Term LTh bnd (LitE (Num 100)))
      return $ inv `Conj` guard `Conj` (Term Eq bnd (LitE (Num 100))) `Implies` pre

precond (Spec pre _) _ = return pre

precondGuard :: Pred -> GdCmd -> M Pred
precondGuard post (GdCmd guard body) = Implies guard <$> precondStmts body post

--- calculating the weakest precondition, and update the syntax tree

sweep :: Stmt -> Pred -> M (Stmt, Pred)

sweep Abort _ = return (Abort, Lit False)

sweep Skip post = return (Skip, post)

sweep (Assert pre) post = do
  unless (predEq pre post) $ do
    obligate $ pre `Implies` post
  return (Assert pre, pre)

sweep (Assign xs es) post =
  return (Assign xs es, substP (Map.fromList (zip xs es)) post)

sweep (If (Just pre) branches) post = do
  branches' <- mapM (sweepGdCmdHard (pre `Conj`) post) branches
  let (guards, _) = unzipGdCmds branches
  obligate $ pre `Implies` disjunct guards
  return (If (Just pre) branches', pre)
sweep (If Nothing branches) post = do
  (branches', brConds) <-
      (unzip <$> mapM (sweepGdCmdSoft post) branches)
  let (guards, _) = unzipGdCmds branches
  return (If Nothing branches', conjunct brConds `Conj` disjunct guards)

sweep (Do inv bnd branches) post = do

  branches' <- mapM (sweepGdCmdHard (inv `Conj`) inv) branches
  mapM_ (obligate <=< termCond) branches'

  let (guards, _) = unzipGdCmds branches'

  obligate $ (inv `Conj` (conjunct (map Neg guards)))
                  `Implies` post -- empty branches?
  obligate $ (inv `Conj` disjunct guards) `Implies` (Term GEq bnd (LitE (Num 0)))

  return (Do inv bnd branches', inv)

  where

    termCond :: GdCmd -> M Pred
    termCond (GdCmd guard body) = do
      pre <- precondStmts body (Term LTh bnd (LitE (Num 100)))
      return $ inv `Conj` guard `Conj` (Term Eq bnd (LitE (Num 100))) `Implies` pre

sweep (Spec p (Hole _)) post = return (Spec p post, p)
sweep (Spec p q) post = do
  unless (predEq q post) $ do
    obligate $ q `Implies` post
  return (Spec p q, p)


sweepStmts :: [Stmt] -> Pred -> M ([Stmt], Pred)

sweepStmts [] post = return ([],post)

sweepStmts (Spec p (Hole _) : xs) post = do
  (xs', post') <- sweepStmts xs post
  return (Spec p post' : xs', p)
sweepStmts (Spec p q : xs) post = do
  (xs', post') <- sweepStmts xs post
  obligate (q `Implies` post')
  return (Spec p q : xs', p)

sweepStmts (Assert p : xs@(_:_)) post = do
  (xs', post') <- sweepStmts xs post
  case xs' of
     [] -> error "shouldn't happen"
     Spec (Hole _) q : xs'' ->
       return (Assert p : Spec p q : xs'', p)
     x : xs'' -> do
       unless (predEq p post') (obligate $ p `Implies` post')
       return (Assert p : x : xs'', p)

sweepStmts (x : xs) post = do
  (xs', post') <- sweepStmts xs post
  (x', pre) <- sweep x post'
  return (x':xs', pre)

sweepGdCmdHard :: (Pred -> Pred) -> Pred -> GdCmd -> M GdCmd
sweepGdCmdHard f post (GdCmd guard body) = do
  (body', _) <- sweepStmts (Assert (f guard) : body) post
  return (GdCmd guard (tail body'))

sweepGdCmdSoft :: Pred -> GdCmd -> M (GdCmd, Pred)
sweepGdCmdSoft post (GdCmd guard body) = do
  (body', pre) <- sweepStmts body post
  return (GdCmd guard body', guard `Implies` pre)

---
gcdExample :: Program
gcdExample = let Right result = abstract $ fromRight $ parseProgram "<test>" "\
  \x := X\n\
  \y := Y\n\
  \{ gcd x y = gcd X Y, bnd: ? }\n\
  \do x > y -> x := minus x y  \n\
  \ | x < y -> y := minus y x  \n\
  \od\n\
  \{ gcd X Y = x }\n\
  \"
  in result

test :: ([Obligation], Pred)
test = swap . runM $ case gcdExample of
  Program _ Nothing -> undefined
  Program _ (Just (statements, postcondition))
    -> precondStmts statements postcondition



-- gcdExample2 :: Stmt
-- gcdExample2 =
--   Assign ["x"] [VarE "X"] `Seq`
--   Assign ["y"] [VarE "Y"] `Seq`
--   Do
--     (Just $ Term Eq (OpE (VarE "gcd") [VarE "x", VarE "y"]) (OpE (VarE "gcd") [VarE "X", VarE "Y"]))
--     (HoleE 0 [])
--     [ GdCmd
--         (Term GTh (VarE "x") (VarE "y"))
--         (Assign ["x"] [OpE (VarE "-") [VarE "x", VarE "y"]])
--     , GdCmd
--         (Term LTh (VarE "x") (VarE "y"))
--         (Assign ["y"] [OpE (VarE "-") [VarE "y", VarE "x"]])
--     ]
--
-- postCond2 :: Pred
-- postCond2 = Term Eq (VarE "x") (OpE (VarE "gcd") [VarE "X", VarE "Y"])
--
-- test2 :: ([Obligation], Pred)
-- test2 = runM $ do
--   precond gcdExample2 postCond2


--

{-
let (stmt', obs, pre)  = runSymbolGen (precond GCL.gcd post)

--
Seq (Seq (Assign ["x"] [Var "X"])
         (Assign ["y"] [Var "Y"]))
 (Do (Term Eq (Op "gcd" [Var "x",Var "y"])
              (Op "gcd" [Var "X",Var "Y"]))
     (HoleE (Just 0))
     [(Term GTh (Var "x") (Var "y"),
         Assign ["x"] [Op "-" [Var "x",Var "y"]]),
      (Term LTh (Var "x") (Var "y"),
         Assign ["y"] [Op "-" [Var "y",Var "x"]])])
--

[(5,"((((gcd x y) = (gcd X Y)) && ((not (x > y)) && (not (x < y)))) => (x = (gcd X Y)))"),
(6,"((((gcd x y) = (gcd X Y)) && ((x > y) || (x < y))) => ([0] >= 0))"),
(1,"((((gcd x y) = (gcd X Y)) && (x > y)) => ((gcd (x - y) y) = (gcd X Y)))"),
(2,"((((gcd x y) = (gcd X Y)) && (x < y)) => ((gcd x (y - x)) = (gcd X Y)))"),
(3,"(((((gcd x y) = (gcd X Y)) && (x > y)) && ([0] = 100)) => ([0] < 100))"),
(4,"(((((gcd x y) = (gcd X Y)) && (x < y)) && ([0] = 100)) => ([0] < 100))")]

-}
