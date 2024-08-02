{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}

module GCL.WP (WP, TM, sweep, structStmts, runWP) where

import           Control.Monad.Except           ( MonadError(throwError)
                                                , runExcept
                                                )
import           Control.Monad.RWS              ( RWST(runRWST) )
import           Data.Text                      ( Text )
import           Data.IntMap                    ( IntMap )
import qualified Data.List                     as List
import           Data.Loc                       ( Located(..) )
import qualified Data.Map                      as Map
import           GCL.Predicate                  ( InfMode(..)
                                                , PO(..)
                                                , Pred
                                                , Spec(..)
                                                )
import           Syntax.Typed
import           Syntax.Typed.Operator          ( true )
import           Syntax.Typed.Util              ( declaredNames
                                                , programToScopeForSubstitution )
import           Syntax.Common.Types            ( nameToText )
import GCL.WP.Types
import GCL.WP.Struct
import GCL.WP.WP
import GCL.WP.SP

runWP
  :: WP a
  -> (Decls, [[Text]])
  -> Int
  -> Either
       StructError
       (a, Int, ([PO], [Spec], [StructWarning], IntMap (Int, Expr)))
runWP p decls counter = runExcept $ runRWST p decls counter

sweep
  :: Program
  -> Either StructError ([PO], [Spec], [StructWarning], IntMap (Int, Expr), Int)
sweep program@(Program _ decs _props stmts _) = do
  let decls = programToScopeForSubstitution program
  let dnames = [map nameToText $ declaredNames decs]
  (_, counter, (pos, specs, warnings, redexes)) <-
           runWP (structProgram stmts) (decls, dnames) 0
  -- update Proof Obligations with corresponding Proof Anchors
  let proofAnchors = stmts >>= \case
        Proof anchor _ r -> [(anchor,r)]
        _                -> []
  -- make a table of (#hash, range) from Proof Anchors
  let table = Map.fromList proofAnchors
  let updatePO po = case Map.lookup (poAnchorHash po) table of
        Nothing    -> po
        Just range -> po { poAnchorLoc = Just range }

  let pos' = map updatePO pos

  return (pos', specs, warnings, redexes, counter)


--------------------------------------------------------------------------------

data ProgView
  = ProgViewEmpty
  | ProgViewOkay Pred [Stmt] Pred
  | ProgViewMissingPrecondition [Stmt] Pred
  | ProgViewMissingPostcondition Pred [Stmt]
  | ProgViewMissingBoth [Stmt]

progView :: [Stmt] -> ProgView
progView []               = ProgViewEmpty
progView [Assert pre _] = do
  ProgViewMissingPrecondition [] pre
progView stmts = do
  case (head stmts, last stmts) of
    (Assert pre _, Assert post _) -> do
      ProgViewOkay pre (init (tail stmts)) post
    (Assert pre _, _) -> do
      ProgViewMissingPostcondition pre (tail stmts)
    (_, Assert post _) -> do
      ProgViewMissingPrecondition (init stmts) post
    _ -> ProgViewMissingBoth stmts

structProgram :: [Stmt] -> WP ()
structProgram stmts = do
  case progView (removeLastProofs stmts) of
    ProgViewEmpty -> return ()
    ProgViewOkay pre stmts' post ->
      structStmts Primary (pre, Nothing) stmts' post
    ProgViewMissingPrecondition stmts' post ->
      structStmts Primary (true, Nothing) stmts' post
    ProgViewMissingPostcondition _ stmts' ->
      throwError . MissingPostcondition . locOf . last $ stmts'
    ProgViewMissingBoth stmts' ->
      throwError . MissingPostcondition . locOf . last $ stmts'

  where
      -- ignore Proofs after the Postcondition
      removeLastProofs :: [Stmt] -> [Stmt]
      removeLastProofs = List.dropWhileEnd isProof

      isProof :: Stmt -> Bool
      isProof Proof{} = True
      isProof _       = False


-- tying the knots

structStmts :: InfMode -> (Pred, Maybe Expr) -> [Stmt] -> Pred -> WP ()
structStmts = this
  where
    (this, structSegs, struct) = structFunctions (wpSegs, wpSStmts,
                                                  wp, spSStmts)
    (wpSegs, wpSStmts, wp)     = wpFunctions structSegs
    spSStmts                   = spFunctions (structSegs, struct)
