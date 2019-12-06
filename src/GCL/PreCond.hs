{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DeriveGeneric #-}

module GCL.PreCond where

import Control.Monad.State hiding (guard)
import Control.Monad.Writer hiding (guard)

import qualified Data.Map as Map
-- import Data.Map (Map)
import Data.Loc (Loc, locOf)
import GHC.Generics

import Syntax.Abstract
import Syntax.Parser

data Obligation = Obligation Index Pred Pred deriving (Show, Generic)
data Hardness = Hard | Soft deriving (Show, Generic)
data Specification = Specification
  { specHardness :: Hardness
  , specPreCond  :: Pred
  , specPostCond :: Pred
  , specLastStmt :: Maybe Loc
  , specLoc      :: Loc
  } deriving (Show, Generic)

type M = WriterT [Obligation] (WriterT [Specification] (State Int))

runM :: M a -> ((a, [Obligation]), [Specification])
runM p = evalState (runWriterT (runWriterT p)) 0

-- creates a proof obligation
obligate :: Pred -> Pred -> M ()
obligate p q = do

  -- NOTE: this could use some love
  let samePredicate = predEq p q

  unless samePredicate $ do
    i <- get
    put (succ i)
    tell [Obligation i p q]

tellSpec :: Hardness -> Pred -> Pred -> [Stmt] -> Loc -> M ()
tellSpec harsness p q [] loc = do
  lift $ tell [Specification harsness p q Nothing loc]
tellSpec harsness p q stmts loc = do
  let lastLoc = locOf $ last stmts
  lift $ tell [Specification harsness p q (Just lastLoc) loc]

conjunct :: [Pred] -> Pred
conjunct []     = Lit True
conjunct [p]    = p
conjunct (p:ps) = p `Conj` conjunct ps

disjunct :: [Pred] -> Pred
disjunct []     = Lit False
disjunct [p]    = p
disjunct (p:ps) = p `Disj` disjunct ps

precondStmts :: [Stmt] -> Pred -> M Pred
precondStmts [] post = return post
precondStmts (x:[]) post = case x of
  -- SOFT
  Spec loc -> do
    tellSpec Soft post post [] loc
    return post
  _ -> do
    precond x post

precondStmts (x:(y:xs)) post = case (x, y) of
  -- HARD
  (Assert asserted _, Spec loc) -> do
    -- calculate the precondition of xs
    post' <- precondStmts xs post

    tellSpec Hard asserted post' [] loc

    obligate asserted post'

    return asserted
  -- SOFT
  (Spec loc, _) -> do
    pre <- precondStmts (y:xs) post
    -- pre <- precondStmts stmts post'
    tellSpec Soft pre pre [] loc
    return pre
  _ -> do
    precondStmts (y:xs) post >>= precond x


-- calculating the weakest precondition
precond :: Stmt -> Pred -> M Pred

precond (Abort _) _ = return (Lit False)

precond (Skip _) post = return post

precond (Assert pre _) post = do
  obligate pre post
  return pre

precond (Assign xs es _) post = return $ substP (Map.fromList (zip xs es)) post

precond (If (Just pre) branches _) post = do


  forM_ branches $ \(GdCmd guard body) -> do
    p <- precondStmts body post
    obligate
      (pre `Conj` guard)    -- HARD precondition AND the guard
      p                     -- precondition of the statements


  let guards = getGuards branches
  obligate
    pre
    (disjunct guards)

  return pre

precond (If Nothing branches _) post = do
  brConds <- mapM (precondGuard post) branches
  let guards = getGuards branches

  return (conjunct brConds `Conj` disjunct guards)

precond (Do inv bnd branches _) post = do

  forM_ branches $ \(GdCmd guard body) -> do
    p <- precondStmts body inv
    obligate
      (inv `Conj` guard)    -- invariant AND the guard
      p                     -- precondition of the statements

  -- termination of each branches
  forM_ branches $ \(GdCmd guard body) -> do
    p <- precondStmts body (Term LTh bnd (LitE (Num 100)))
    obligate
      -- invariant AND the guard AND some hard limit
      (inv `Conj` guard `Conj` (Term Eq bnd (LitE (Num 100))))
      -- precondition of the statements
      p

  let guards = getGuards branches

  -- after the loop, the invariant should still hold and all guards should fail
  obligate
    (inv `Conj` (conjunct (map Neg guards)))
    post -- empty branches?

  -- termination of the whole statement
  obligate
    (inv `Conj` disjunct guards)
    (Term GEq bnd (LitE (Num 0)))

  return inv

precond (Spec _) post = return post

precondGuard :: Pred -> GdCmd -> M Pred
precondGuard post (GdCmd guard body) = Implies guard <$> precondStmts body post

--- calculating the weakest precondition, and update the syntax tree

-- sweep :: Stmt -> Pred -> M (Stmt, Pred)
--
-- sweep Abort _ = return (Abort, Lit False)
--
-- sweep Skip post = return (Skip, post)
--
-- sweep (Assert pre) post = do
--   unless (predEq pre post) $ do
--     obligate $ pre `Implies` post
--   return (Assert pre, pre)
--
-- sweep (Assign xs es) post =
--   return (Assign xs es, substP (Map.fromList (zip xs es)) post)
--
-- sweep (If (Just pre) branches) post = do
--   branches' <- mapM (sweepGdCmdHard (pre `Conj`) post) branches
--   let guards = getGuards branches
--   obligate $ pre `Implies` disjunct guards
--   return (If (Just pre) branches', pre)
-- sweep (If Nothing branches) post = do
--   (branches', brConds) <-
--       (unzip <$> mapM (sweepGdCmdSoft post) branches)
--   let guards = getGuards branches
--   return (If Nothing branches', conjunct brConds `Conj` disjunct guards)
--
-- sweep (Do inv bnd branches) post = do
--
--   branches' <- mapM (sweepGdCmdHard (inv `Conj`) inv) branches
--   mapM_ (obligate <=< termCond) branches'
--
--   let guards = getGuards branches'
--
--   obligate $ (inv `Conj` (conjunct (map Neg guards)))
--                   `Implies` post -- empty branches?
--   obligate $ (inv `Conj` disjunct guards) `Implies` (Term GEq bnd (LitE (Num 0)))
--
--   return (Do inv bnd branches', inv)
--
--   where
--
--     termCond :: GdCmd -> M Pred
--     termCond (GdCmd guard body) = do
--       pre <- precondStmts body (Term LTh bnd (LitE (Num 100)))
--       return $ inv `Conj` guard `Conj` (Term Eq bnd (LitE (Num 100))) `Implies` pre
--
-- sweep (Spec stmts p (Hole _) loc) post = return (Spec stmts p post loc, p)
-- sweep (Spec stmts p q loc) post = do
--   unless (predEq q post) $ do
--     obligate $ q `Implies` post
--   return (Spec stmts p q loc, p)
--
--
-- sweepStmts :: [Stmt] -> Pred -> M ([Stmt], Pred)
--
-- sweepStmts [] post = return ([],post)
--
-- sweepStmts (Spec stmts p (Hole _) loc : xs) post = do
--   (xs', post') <- sweepStmts xs post
--   -- tellSpec (Just p) post' loc
--   return (Spec stmts p post' loc : xs', p)
-- sweepStmts (Spec stmts p q loc : xs) post = do
--   (xs', post') <- sweepStmts xs post
--   -- tellSpec Nothing post' loc
--   obligate (q `Implies` post')
--   return (Spec stmts p q loc : xs', p)
--
-- sweepStmts (Assert p : xs@(_:_)) post = do
--   (xs', post') <- sweepStmts xs post
--   case xs' of
--      [] -> error "shouldn't happen"
--      Spec stmts (Hole _) q loc : xs'' -> do
--        -- tellSpec (Just p) q loc
--        return (Assert p : Spec stmts p q loc : xs'', p)
--      x : xs'' -> do
--        unless (predEq p post') (obligate $ p `Implies` post')
--        return (Assert p : x : xs'', p)
--
-- sweepStmts (x : xs) post = do
--   (xs', post') <- sweepStmts xs post
--   (x', pre) <- sweep x post'
--   return (x':xs', pre)
--
-- sweepGdCmdHard :: (Pred -> Pred) -> Pred -> GdCmd -> M GdCmd
-- sweepGdCmdHard f post (GdCmd guard body) = do
--   (body', _) <- sweepStmts (Assert (f guard) : body) post
--   return (GdCmd guard (tail body'))
--
-- sweepGdCmdSoft :: Pred -> GdCmd -> M (GdCmd, Pred)
-- sweepGdCmdSoft post (GdCmd guard body) = do
--   (body', pre) <- sweepStmts body post
--   return (GdCmd guard body', guard `Implies` pre)

gcdExample :: Either SyntaxError Program
gcdExample = do
  result <- parseProgram "<test>" "\
    \x := X\n\
    \y := Y\n\
    \{ gcd(x, y) = gcd(X, Y), bnd: ? }\n\
    \do x > y -> x := minus(x, y)  \n\
    \ | x < y -> y := minus(y, x)  \n\
    \od\n\
    \{ gcd(X, Y) = x }\n\
    \"
  abstract result

test :: ((Pred, [Obligation]), [Specification])
test = runM $ case gcdExample of
  Right (Program _ (Just (statements, postcondition)))
    -> precondStmts statements postcondition
  _ -> undefined
