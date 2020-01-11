{-# LANGUAGE OverloadedStrings, FlexibleContexts, DeriveGeneric,
             TypeSynonymInstances, FlexibleInstances #-}

module GCL.PreCond where

import Control.Monad.State hiding (guard)
import Control.Monad.Writer hiding (guard)

import Data.Loc (Loc(..))
import GHC.Generics

import Syntax.Concrete
import Syntax.Abstract (Fresh(..))
import qualified Syntax.Abstract as A

data Obligation = Obligation Index Pred Pred deriving (Show, Generic)
data Hardness = Hard | Soft deriving (Show, Generic)
data Specification = Specification
  { specID       :: Int
  , specHardness :: Hardness
  , specPreCond  :: Pred
  , specPostCond :: Pred
  , specLoc      :: Loc
  } deriving (Show, Generic)

type M = WriterT [Obligation] (WriterT [Specification] (State (Int, Int, Int)))

instance Fresh M where
  fresh = do (i, j, k) <- get
             put (i, j, succ k)
             return k

runM :: M a -> ((a, [Obligation]), [Specification])
runM p = evalState (runWriterT (runWriterT p)) (0, 0, 0)

abstractA :: A.FromConcrete a p => a -> p
abstractA e = case A.abstract e of
   Right e' -> e'
   Left _ -> error "internal error while converting to abs"

-- SCM: I thought these would be useful,
--        but it turns out that I do not need them yet.

censorObli :: ([Obligation] -> [Obligation]) -> M a -> M a
censorObli = censor

censorSpec :: ([Specification] -> [Specification]) -> M a -> M a
censorSpec f = mapWriterT (censor f)

type Pred = A.Expr
type Index = A.Index

-- creates a proof obligation
obligate :: Pred -> Pred -> M ()
obligate p q = do

  -- NOTE: this could use some love
  let samePredicate = A.predEq p q

  unless samePredicate $ do
    (i, j, k) <- get
    put (succ i, j, k)
    tell [Obligation i p q]

tellSpec :: Hardness -> Pred -> Pred -> Loc -> M ()
tellSpec harsness p q loc = do
  (i, j, k) <- get
  put (i, succ j, k)
  lift $ tell [Specification j harsness p q loc]


structure :: Bool -> Pred -> Maybe (A.Expr) -> Stmt -> Pred -> M ()

structure _ pre _ (Abort _) _ = obligate pre (A.Lit (A.Bol False))

structure _ pre _ (Skip _) post = obligate pre post

structure _ pre _ (Assert p _) post =
   let p' = abstractA p
   in obligate pre p' >> obligate p' post

structure _ pre _ (AssertWithBnd p _ _) post = -- shouldn't happen
   let p' = abstractA p
   in obligate pre p' >> obligate p' post

structure _ pre _ (Assign xs es _) post = do
  post' <- A.subst (zip (map lowerToText xs)
                        (map abstractA es)) post
  obligate pre post'

structure b pre _ (If gcmds _) post = do
  let guards = map abstractA (getGuards gcmds)
  obligate pre (A.disjunct guards)
  forM_ gcmds $ \(GdCmd guard body _) ->
    structStmts b (pre `A.conj` abstractA guard) Nothing body post

structure b inv Nothing (Do gcmds _) post = do -- warn that bnd is missing
  let gcmds' = map (\(GdCmd x y _) -> (abstractA x, y)) gcmds
  let guards = map fst gcmds'
  obligate (inv `A.conj` (A.conjunct (map A.neg guards))) post
  --
  forM_ gcmds' $ \(guard, body) -> do
    structStmts b (inv `A.conj` guard) Nothing body inv

structure b inv (Just bnd) (Do gcmds _) post = do
  let gcmds' = map (\(GdCmd x y _) -> (abstractA x, y)) gcmds
  let guards = map fst gcmds'
  obligate (inv `A.conj` (A.conjunct (map A.neg guards))) post
  --
  forM_ gcmds' $ \(guard, body) -> do
    structStmts b (inv `A.conj` guard) Nothing body inv
  --
  obligate (inv `A.conj` A.disjunct guards) (bnd `A.gte` (A.Lit (A.Num 0)))
  --
  oldbnd <- freshVar "bnd"
  let invB = inv `A.conj` (bnd `A.eqq` A.Var oldbnd)
  forM_ gcmds' $ \(guard, body) -> do
    structStmts False (inv `A.conj` guard) Nothing body invB

structure b pre _ (SpecQM l) post = when b (tellSpec Soft pre post l)
structure b pre _ (Spec l) post = when b (tellSpec Soft pre post l)


structStmts :: Bool -> Pred -> Maybe (A.Expr) -> [Stmt] -> Pred -> M ()

structStmts _ _ _ [] _ = return ()

structStmts b pre _ (Assert p _ : stmts) post =
  let p' = abstractA p in
  obligate pre p' >>
  structStmts b p' Nothing stmts post

structStmts b pre _ (AssertWithBnd p bnd _ : stmts) post =
  let (p', bnd') = (abstractA p, abstractA bnd) in
  obligate pre p' >>
  structStmts b p' (Just bnd') stmts post

structStmts b pre bnd (stmt : stmts) post = do
  post' <- wpStmts b stmts post
  structure b pre bnd stmt post'


wpStmts :: Bool -> [Stmt] -> Pred -> M Pred

wpStmts _ [] post = return post

wpStmts b (Assert pre _ : stmts) post =
  let pre' = abstractA pre in
  structStmts b pre' Nothing stmts post >>
  return pre'

wpStmts b (AssertWithBnd pre bnd _ : stmts) post =
  let (pre', bnd') = (abstractA pre, abstractA bnd) in
  structStmts b pre' (Just bnd') stmts post >>
  return pre'

wpStmts b (stmt : stmts) post = do
  post' <- wpStmts b stmts post
  wp b stmt post'

wp :: Bool -> Stmt -> Pred -> M Pred

wp _ (Abort _) _ = return (A.Lit (A.Bol False))

wp _ (Skip _) post = return post

wp _ (Assert p _) post =
   let p' = abstractA p
   in obligate p' post >> return p'

wp _ (AssertWithBnd p _ _) post =
  let p' = abstractA p
  in obligate p' post >> return p'

wp _ (Assign xs es _) post =
  A.subst (zip (map lowerToText xs) (map abstractA es)) post

wp b (If gcmds _) post = do
    forM_ gcmds $ \(GdCmd guard body _) ->
      structStmts b (abstractA guard) Nothing body post
    let guards = map abstractA (getGuards gcmds)
    return (A.disjunct guards) -- is this enough?

wp b (Do gcmds _) post = undefined

wp b (SpecQM l) post =
  when b (tellSpec Soft post post l) >> return post  -- not quite right

wp b (Spec l) post =
  when b (tellSpec Soft post post l) >> return post  -- not quite right
