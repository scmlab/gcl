{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}

module GCL.WP (WP, TM, sweep) where

import           Control.Monad.Except           ( MonadError(throwError)
                                                , runExcept
                                                )
import           Control.Monad.RWS              ( RWST(runRWST) )
import           Data.IntMap                    ( IntMap )
import qualified Data.List                     as List
import           Data.Loc                       ( Located(..) )
import qualified Data.Map                      as Map
import           GCL.Predicate                  ( InfMode(..)
                                                , PO(..)
                                                , Pred(..)
                                                , Spec(..)
                                                )
import qualified GCL.Substitution              as Substitution
import           GCL.WP.Type
import qualified Syntax.Abstract               as A
import qualified Syntax.Abstract.Operator      as A
import qualified Syntax.Abstract.Util          as A
import GCL.WP.Struct
import GCL.WP.WP
import GCL.WP.SP

runWP
  :: WP a
  -> Substitution.Scope
  -> Either
       StructError
       (a, Int, ([PO], [Spec], [StructWarning], IntMap (Int, A.Expr)))
runWP p decls = runExcept $ runRWST p decls 0

sweep
  :: A.Program
  -> Either StructError ([PO], [Spec], [StructWarning], IntMap (Int, A.Expr), Int)
sweep program@(A.Program _ _ _props stmts _) = do
  let scope = A.programToScopeForSubstitution program
  (_, counter, (pos, specs, warnings, redexes)) <- runWP (structProgram stmts)
                                                         scope
  -- update Proof Obligations with corresponding Proof Anchors
  let proofAnchors = stmts >>= \case
        A.Proof anchors _ -> anchors
        _                 -> []
  -- make a table of (#hash, range) from Proof Anchors
  let table = Map.fromList
        $ map (\(A.ProofAnchor hash range) -> (hash, range)) proofAnchors
  let updatePO po = case Map.lookup (poAnchorHash po) table of
        Nothing    -> po
        Just range -> po { poAnchorLoc = Just range }

  let pos' = map updatePO pos

  return (pos', specs, warnings, redexes, counter)


--------------------------------------------------------------------------------

data ProgView
  = ProgViewEmpty
  | ProgViewOkay Pred [A.Stmt] Pred
  | ProgViewMissingPrecondition [A.Stmt] Pred
  | ProgViewMissingPostcondition Pred [A.Stmt]
  | ProgViewMissingBoth [A.Stmt]

progView :: [A.Stmt] -> ProgView
progView []               = ProgViewEmpty
progView [A.Assert pre l] = do
  ProgViewMissingPrecondition [] (Assertion pre l)
progView stmts = do
  case (head stmts, last stmts) of
    (A.Assert pre l, A.Assert post m) -> do
      ProgViewOkay (Assertion pre l)
                   (init (tail stmts))
                   (Assertion post m)
    (A.Assert pre l, _) -> do
      ProgViewMissingPostcondition (Assertion pre l)
                                   (tail stmts)
    (_, A.Assert post m) -> do
      ProgViewMissingPrecondition (init stmts) (Assertion post m)
    _ -> ProgViewMissingBoth stmts

structProgram :: [A.Stmt] -> WP ()
structProgram stmts = do
  case progView (removeLastProofs stmts) of
    ProgViewEmpty -> return ()
    ProgViewOkay pre stmts' post ->
      structStmts Primary (pre, Nothing) stmts' post
    ProgViewMissingPrecondition stmts' post ->
      structStmts Primary (Constant A.true, Nothing) stmts' post
    ProgViewMissingPostcondition _ stmts' ->
      throwError . MissingPostcondition . locOf . last $ stmts'
    ProgViewMissingBoth stmts' ->
      throwError . MissingPostcondition . locOf . last $ stmts'

  where
      -- ignore Proofs after the Postcondition
      removeLastProofs :: [A.Stmt] -> [A.Stmt]
      removeLastProofs = List.dropWhileEnd isProof

      isProof :: A.Stmt -> Bool
      isProof A.Proof{} = True
      isProof _         = False


-- tying the knots

structStmts :: InfMode -> (Pred, Maybe A.Expr) -> [A.Stmt] -> Pred -> WP ()
structStmts = this
  where
    (this, structSegs, struct) = structFunctions (wpSegs, wpSStmts,
                                                  wp, spSStmts)
    (wpSegs, wpSStmts, wp)     = wpFunctions structSegs
    spSStmts                   = spFunctions (structSegs, struct)
