{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DeriveGeneric #-}

module GCL.PreCond2 where

import Control.Monad.Free
import Control.Monad.State hiding (guard)
import Control.Monad.Writer hiding (guard)

import GHC.Generics

import Syntax.Abstract hiding (Stmt(..), GdCmd(..), Program(..), getGuards)

import Syntax.Lasagne

data Obligation = Obligation Index Pred Pred deriving (Show, Generic)
-- data Specification = Specification
--   { specID       :: Int
--   , specHardness :: Hardness
--   , specPreCond  :: Pred
--   , specPostCond :: Pred
--   , specLoc      :: Loc
--   } deriving (Show, Generic)

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

runPOM :: POM a -> (a, [Obligation])
runPOM p = evalState (runWriterT p) 0



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
