{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}

module GCL where

import Control.Monad.State hiding (guard)
import Control.Monad.Writer hiding (guard)

import qualified Data.Map as Map
-- import Data.Map (Map)
import Data.Tuple (swap)

import Syntax.Abstract
import Syntax.Parser

data Obligation = Obligation Index Pred deriving (Show)

type M = WriterT [Obligation] (State Int)

runM :: M Pred -> ([Obligation], Pred)
runM p = evalState (swap <$> runWriterT p) 0

-- creates a proof obligation
shouldProof :: Pred -> M ()
shouldProof p = do
  i <- get
  put (succ i)
  tell [Obligation i p]

conjunct :: [Pred] -> Pred
conjunct = foldr Conj (Lit False)

disjunct :: [Pred] -> Pred
disjunct = foldr Disj (Lit True)

precondStmts :: Stmts -> Pred -> M Pred
precondStmts (Postcondition _) post = return post
precondStmts (Seq Nothing stmt q) post = do
  -- no precondition asserted, return the computed one
  precondStmts q post >>= precond stmt Nothing
precondStmts (Seq (Just pre) stmt q) post = do
  -- conpute the precondition of (stmt; q)
  post' <- precondStmts q post >>= precond stmt (Just pre)

  -- generate a proof obligation,
  -- if the precondition doesn't coincides with the postcondition
  unless (predEq pre post') $ do
    shouldProof $ pre `Implies` post'

  return pre

-- calculating the weakest precondition
precond :: Stmt -> Maybe Pred -> Pred -> M Pred

precond Abort _ _ = undefined

precond Skip _ post = return post

precond (Assign xs es) _ post = return $ substP (Map.fromList (zip xs es)) post

precond (If branches) (Just pre) post = do
  mapM_ (shouldProof <=< obliGuard pre post) branches
  let (guards, _) = unzipGdCmds branches
  shouldProof $ pre `Implies` disjunct guards
  return pre
  where
    obliGuard :: Pred -> Pred -> GdCmd -> M Pred
    obliGuard pre' post' (GdCmd guard body) = Implies (pre' `Conj` guard) <$> precondStmts body post'
precond (If branches) Nothing post = do
  brConds <- mapM (precondGuard post) branches
  let (guards, _) = unzipGdCmds branches

  return (conjunct brConds `Conj` disjunct guards)

precond (Do _ _) Nothing _ = undefined
precond (Do bnd branches) (Just inv)  post = do

  mapM_ (shouldProof <=< branchCond) branches
  mapM_ (shouldProof <=< termCond) branches

  let (guards, _) = unzipGdCmds branches

  shouldProof $ (inv `Conj` (conjunct (map Neg guards)))
                  `Implies` post -- empty branches?
  shouldProof $ (inv `Conj` disjunct guards) `Implies` (Term GEq bnd (LitE (Num 0)))

  return inv

  where
    branchCond :: GdCmd -> M Pred
    branchCond (GdCmd guard body) = Implies (inv `Conj` guard) <$> precondStmts body inv

    termCond :: GdCmd -> M Pred
    termCond (GdCmd guard body) = do
      pre <- precondStmts body (Term LTh bnd (LitE (Num 100)))
      return $ inv `Conj` guard `Conj` (Term Eq bnd (LitE (Num 100))) `Implies` pre

precond (Spec pre pos) _ post
  | predEq pos post = return pre
  | otherwise = do
       shouldProof (pos `Implies` post)
       return pre

precondGuard :: Pred -> GdCmd -> M Pred
precondGuard post (GdCmd guard body) = Implies guard <$> precondStmts body post

gcdExample :: Program
gcdExample = abstract $ fromRight $ parseProgram "<test>" "\
  \x := X\n\
  \y := Y\n\
  \{ gcd x y = gcd X Y }\n\
  \do { ? } \n\
  \  | x > y -> x := minus x y  \n\
  \  | x < y -> y := minus y x  \n\
  \od\n\
  \"

postCond :: Pred
postCond = abstract $ fromRight $ parsePred "gcd X Y = x"

test :: ([Obligation], Pred)
test = runM $ do
  let Program _ statements = gcdExample
  precondStmts statements postCond


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
