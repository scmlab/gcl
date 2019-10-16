{-# LANGUAGE FlexibleContexts #-}

module GCL where

import Control.Monad.State hiding (guard)
import Control.Monad.Writer hiding (guard)

import qualified Data.Map as Map
-- import Data.Map (Map)
import Data.Tuple (swap)

import GCL.Expr
import GCL.Pred
import GCL.Stmt
import GCL.EnumHole

data Obligation = Obligation Idx Pred deriving (Show)

type M = WriterT [Obligation] (State Int)

runM :: M Pred -> ([Obligation], Pred)
runM p = evalState (swap <$> runWriterT p) 0

-- creates a proof obligation
shouldProof :: Pred -> M ()
shouldProof p = do
  i <- get
  put (succ i)
  tell [Obligation i p]

precond :: Stmt -> Pred -> M Pred
-- precond :: (MonadSymGen Idx m) => Stmt -> Pred -> m ([(Idx, Pred)], Pred)
precond Skip post = return post
precond (Assign xs es) post = return $ substP (Map.fromList (zip xs es)) post
precond (Seq c1 c2) post = precond c2 post >>= precond c1
precond (Assert p) post = do
  shouldProof $ p `Implies` post
  return p
precond (If _branches) _post = undefined
 {-  where guards = map fst branches
        bodies = map snd branches
        conds = map (flip precond post) bodies -}
precond (Do inv bnd branches) post = do

  mapM_ (shouldProof <=< branchCond) branches
  mapM_ (shouldProof <=< termCond) branches

  let (guards, _bodies) = unzipBranches branches

  shouldProof $ (inv `Conj` (foldr1 Conj (map Neg guards)))
                  `Implies` post -- empty branches?
  shouldProof $ (inv `Conj` foldr1 Disj guards) `Implies` (Term GEq bnd (Lit (Num 0)))

  return inv

  where
    unzipBranches :: [Branch] -> ([Pred], [Stmt])
    unzipBranches = unzip . map (\(Branch x y) -> (x, y))

    branchCond :: Branch -> M Pred
    branchCond (Branch guard body) = Implies (inv `Conj` guard) <$> precond body inv

    termCond :: Branch -> M Pred
    termCond (Branch guard body) = do
      pre <- precond body (Term LTh bnd (Lit (Num 100)))
      return $ inv `Conj` guard `Conj` (Term Eq bnd (Lit (Num 100))) `Implies` pre

gcdExample :: Stmt
gcdExample =
  Assign ["x"] [Var "X"] `Seq`
  Assign ["y"] [Var "Y"] `Seq`
  Do
    (Term Eq (Op "gcd" [Var "x", Var "y"]) (Op "gcd" [Var "X", Var "Y"]))
    (HoleE Nothing [])
    [ Branch
        (Term GTh (Var "x") (Var "y"))
        (Assign ["x"] [Op "-" [Var "x", Var "y"]])
    , Branch
        (Term LTh (Var "x") (Var "y"))
        (Assign ["y"] [Op "-" [Var "y", Var "x"]])
    ]

postCond :: Pred
postCond = Term Eq (Var "x") (Op "gcd" [Var "X", Var "Y"])

test :: ([Obligation], Pred)
test = runM $ do
  let gcd' = runEnumHole gcdExample
  precond gcd' postCond

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
