{-# LANGUAGE FlexibleContexts #-}

module GCL where

import Control.Arrow ((***))
import Control.Monad.State

import qualified Data.Map as Map
-- import Data.Map (Map)

import GCL.Expr
import GCL.Pred
import GCL.Stmt
import GCL.EnumHole

type M = State Int

runM :: M a -> a
runM p = evalState p 0

gensym :: M Idx
gensym = do
  i <- get
  put (succ i)
  return i


precond :: Stmt -> Pred -> M ([(Idx, Pred)], Pred)
-- precond :: (MonadSymGen Idx m) => Stmt -> Pred -> m ([(Idx, Pred)], Pred)
precond Skip _post =
   return ([], _post)
precond (Assign xs es) _post =
   return ([], substP (Map.fromList (zip xs es)) post)
precond (Seq c1 c2) _post =
   do (obs2, pre ) <- precond c2 post
      (obs1, pre') <- precond c1 pre
      return (obs1 ++ obs2, pre')
precond (Assert p) _post =
  do i <- gensym
     return ([(i, p `Implies` post)], p)
precond (If _branches) _post =
   undefined
 {-  where guards = map fst branches
        bodies = map snd branches
        conds = map (flip precond post) bodies -}
precond (Do inv bnd branches) _post =
   do (obs, brConds) <-
          (concat *** id) . unzip <$> mapM branchCond branches
      (obsT, termConds2) <-
          (concat *** id) . unzip <$> mapM termCond2 branches
      brConds'    <- enumWithIdx brConds
      termConds2' <- enumWithIdx termConds2
      i1 <- gensym
      i2 <- gensym
      return ((i1, baseCond) : (i2, termCond1) :
              brConds' ++ termConds2' ++ obs ++ obsT
             , inv)
  where (guards, _bodies) = unzip $ map (\(Branch x y) -> (x, y)) branches
        baseCond = (inv `Conj` (foldr1 Conj (map Neg guards)))
                      `Implies` post -- empty branches?
        branchCond :: Branch -> M ([(Idx, Pred)], Pred)
        branchCond (Branch guard body) =
          (id *** Implies (inv `Conj` guard)) <$>
              precond body inv

        termCond1 = (inv `Conj` foldr1 Disj guards) `Implies`
                      (Term GEq bnd (Lit (Num 0)))
        termCond2 (Branch guard body) =
          do (obs, pre) <- precond body (Term LTh bnd (Lit (Num 100)))
             return (obs,
               (inv `Conj` guard `Conj` (Term Eq bnd (Lit (Num 100))))
                 `Implies` pre)

enumWithIdx :: [a] -> M [(Idx,a)]
enumWithIdx [] = return []
enumWithIdx (p:ps) = do
  i <- gensym
  ps' <- enumWithIdx ps
  return ((i,p):ps')
---

gcdExample :: Stmt
gcdExample = Assign ["x"] [Var "X"] `Seq`
      Assign ["y"] [Var "Y"] `Seq`
      Do (Term Eq (Op "gcd" [Var "x", Var "y"])
                  (Op "gcd" [Var "X", Var "Y"]))
         (HoleE Nothing [])
  [ Branch
      (Term GTh (Var "x") (Var "y"))
      (Assign ["x"] [Op "-" [Var "x", Var "y"]])
  , Branch
      (Term LTh (Var "x") (Var "y"))
      (Assign ["y"] [Op "-" [Var "y", Var "x"]])
  ]

post :: Pred
post = (Term Eq (Var "x")
                (Op "gcd" [Var "X", Var "Y"]))

test :: ([(Idx, Pred)], Pred)
test = runM $ do
  let gcd' = runEnumHole gcdExample
  precond gcd' post

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
