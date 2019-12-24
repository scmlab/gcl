{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DeriveGeneric #-}

module GCL.PreCond2 where

import Control.Monad.Free
import Control.Monad.State hiding (guard)
import Control.Monad.Writer hiding (guard)
import Data.Loc
-- import GHC.Generics

import Syntax.Abstract hiding (Stmt(..), GdCmd(..), Program(..), getGuards)
import Syntax.Lasagne
import GCL.PreCond (Obligation(..), Specification(..), Hardness(..))

--------------------------------------------------------------------------------
-- | Proof Obligation

-- data Obligation = Obligation Index Pred Pred deriving (Show, Generic)

-- marks a proof obligation
markPO :: Pred -> Pred -> POM ()
markPO p q = do

  -- NOTE: this could use some love
  let samePredicate = predEq p q

  unless samePredicate $ do
    i <- get
    put (succ i)
    tell [Obligation i p q]

type POM = WriterT [Obligation] (State Int)

runPOM :: POM a -> [Obligation]
runPOM p = snd $ evalState (runWriterT p) 0

getPOs :: Program -> [Obligation]
getPOs = runPOM . sweepPOs

-- generates proof obligations
sweepPOs :: Program -> POM ()
sweepPOs (Pure _) = return ()
sweepPOs (Free (Sauce post _ stmt)) = case stmt of
  Skip xs -> sweepPOs xs
  Abort xs -> sweepPOs xs
  Assign _ _ xs -> sweepPOs xs

  Assert pre xs -> do
    markPO pre post
    sweepPOs xs

  If (Just pre) branches xs -> do

    forM_ branches $ \(GdCmd guard body) -> do
      markPO
        (pre `conj` guard)    -- HARD precondition AND the guard
        (precond body)        -- precondition of the statements

    markPO
      pre
      (disjunct (getGuards branches))

    sweepPOs xs

  If Nothing _ xs -> sweepPOs xs

  Do inv bnd branches xs -> do

    forM_ branches $ \(GdCmd guard body) -> do
      markPO
        (inv `conj` guard)            -- invariant AND the guard
        (recalculatePrecond body inv) -- precondition of the statements

    -- termination of each branches
    forM_ branches $ \(GdCmd guard body) -> do

      markPO
        -- invariant AND the guard AND some hard limit
        (inv `conj` guard `conj` (bnd `eqq` (Lit (Num 100))))
        -- precondition of the statements
        (recalculatePrecond body (bnd `lt` (Lit (Num 100))))

    let guards = getGuards branches

    -- after the loop, the invariant should still hold and all guards should fail
    markPO
      (inv `conj` (conjunct (map neg guards)))
      post -- empty branches?

    -- termination of the whole statement
    markPO
      (inv `conj` disjunct guards)
      (bnd `gte` (Lit (Num 0)))

    sweepPOs xs

  Spec xs -> sweepPOs xs

--------------------------------------------------------------------------------
-- | Specifications

type SpecM = WriterT [Specification]
  (State
    (Int,         -- Spec IDs
    Maybe Pred))  -- previous assertion

-- marks a proof specification
markSpec :: Hardness -> Pred -> Pred -> Loc -> SpecM ()
markSpec harsness p q loc = do
  (i, e) <- get
  put (succ i, e)
  tell [Specification i harsness p q loc]

runSpecM :: SpecM a -> [Specification]
runSpecM p = snd $ evalState (runWriterT p) (0, Nothing)

getSpecs :: Program -> [Specification]
getSpecs = runSpecM . sweepSpecs

-- generates specifications
sweepSpecs :: Program -> SpecM ()
sweepSpecs program = do

  case program of
    -- remember the current assertion
    Free (Sauce _ _ (Assert asserted _)) -> do
      -- replace the previous assertion with the current one
      (i, _) <- get
      put (i, Just asserted)

    -- see if the previous statement is an assertion
    Free (Sauce _ loc (Spec next)) -> do
      (i, previous) <- get

      case previous of
        -- SOFT, the previous statement is not an assertion
        Nothing -> markSpec Soft (precond next) (precond next) loc
        -- HARD, the previous statement is an assertion
        Just asserted -> markSpec Hard asserted (precond next) loc

      put (i, Nothing)

    -- erase the previous remembered assertion if there's any
    _ -> do
      (i, _) <- get
      put (i, Nothing)

  -- keep sweeping down
  case skipToNext program of
    Nothing -> return ()
    Just next -> sweepSpecs next
