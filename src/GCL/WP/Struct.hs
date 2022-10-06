{-# LANGUAGE OverloadedStrings #-}

module GCL.WP.Struct where

import           Control.Monad.Except           ( forM_ )
import           Data.Loc                       ( Loc(..)
                                                , Located(..)
                                                )
import           Data.Loc.Range                 ( fromLoc )
import           GCL.Predicate                  ( InfMode(..)
                                                , Origin(..)
                                                , Pred(..)
                                                )
import           GCL.Predicate.Util             ( guardIf
                                                , guardLoop
                                                )
import GCL.WP.Type
import           GCL.WP.Explanation
import GCL.WP.Util
import qualified Syntax.Abstract               as A
import qualified Syntax.Abstract.Operator      as A
import qualified Syntax.Abstract.Util          as A

type RecFns = (TwpSegs, TwpSStmts, Twp, TspSStmts)

structFunctions :: (TwpSegs, TwpSStmts, Twp, TspSStmts)
                -> (TstructStmts, TstructSegs, Tstruct)
structFunctions (wpSegs, wpSStmts, wp, spSStmts) =
   (structStmts, structSegs, struct)

 where

 structStmts :: InfMode -> (Pred, Maybe A.Expr) -> [A.Stmt] -> Pred -> WP ()
 structStmts Primary pre stmts post = structSegs pre (groupStmts stmts) post
 structStmts Secondary (pre, _) stmts post = case stripAsserts stmts of
   Nothing     -> return ()  -- skip if the program is incomplete
   Just stmts' -> do
     post' <- wpSStmts stmts' post
     tellPO
       pre
       post'
       (emptyExplain "Assertion (Secondary)" (locOf pre))

 structSegs :: (Pred, Maybe A.Expr) -> [SegElm] -> Pred -> WP ()
 structSegs (pre, _) [] post = do
  case locOf pre of
    NoLoc  -> tellPO pre post (AtAssertion (locOf post))
    others -> tellPO pre post (AtAssertion others)
 structSegs (pre, _) (SAsrt (A.Assert p l) : segs) post = do
   let assert = Assertion p l
   tellPO pre assert (AtAssertion (locOf pre))
   structSegs (assert, Nothing) segs post
 structSegs (pre, _) (SAsrt (A.LoopInvariant p bnd l) : segs) post = do
  let loopInv = LoopInvariant p bnd l
  tellPO pre loopInv origin
  structSegs (loopInv, Just bnd) segs post
  where
   startsWithDo :: [SegElm] -> Bool
   startsWithDo (SStmts (A.Do _ _ : _) : _) = True
   startsWithDo _                           = False
   origin = if startsWithDo segs then AtLoop l else AtAssertion l
 structSegs (pre, bnd) [SStmts ss] post = structSStmts (pre, bnd) ss post
 structSegs (pre, bnd) (SStmts ss : SAsrt (A.Assert p l) : segs) post = do
  structSStmts (pre, bnd) ss (Assertion p l)
  structSegs (Assertion p l, Nothing) segs post
 structSegs (pre, bnd) (SStmts ss : SAsrt (A.LoopInvariant p bd l) : segs) post
  = do
    structSStmts (pre, bnd) ss (LoopInvariant p bd l)
    structSegs (LoopInvariant p bd l, Just bd) segs post
 structSegs (pre, bnd) (SStmts ss : SSpec (A.Spec _ range) : segs) post = do
  post' <- wpSegs segs post
  pre'  <- spSStmts (pre, bnd) ss
  tellSpec pre' post' range
 structSegs (pre, _) (SSpec (A.Spec _ range) : segs) post = do
  post' <- wpSegs segs post
  tellSpec pre post' range
 structSegs _ _ _ = error "Missing case in structSegs"

 -- 'simple' version of struct stmts -- there are no assertions,
 -- invariants, or specs in the list of statements.

 structSStmts ::  (Pred, Maybe A.Expr) -> [A.Stmt] -> Pred -> WP ()
 structSStmts (pre, _) [] post = do
   case locOf pre of
     NoLoc  -> tellPO pre post (AtAssertion (locOf post))
     others -> tellPO pre post (AtAssertion others)
 structSStmts (pre, bnd) (stmt : stmts) post = do
   post' <- wpSStmts stmts post
   struct (pre, bnd) stmt post'

 struct :: (Pred, Maybe A.Expr) -> A.Stmt -> Pred -> WP ()
 struct (pre, _) s@(A.Abort l) post = tellPO' (AtAbort l) pre =<< wp s post
 struct (pre, _) s@(A.Skip l) post = tellPO' (AtSkip l) pre =<< wp s post
 struct (pre, _) s@(A.Assign vars exprs l) post = do
   tellPO' origin pre =<< wp s post
  where
   origin :: Origin
   origin = explainAssignment pre post vars exprs l
 struct (pre, _) s@(A.AAssign _ _ _ l) post = do
   tellPO' (AtAssignment l) pre =<< wp s post
 struct (pre, _) (A.If gcmds l) post = do
   tellPO pre (disjunctGuards gcmds) (AtIf l)
   forM_ gcmds $ \(A.GdCmd guard body _) ->
     structStmts Primary (Conjunct [pre, guardIf guard], Nothing) body post
 struct (inv, Just bnd) (A.Do gcmds l) post = do
  let guards = A.getGuards gcmds
  tellPO (Conjunct (inv : map (Negate . guardLoop) guards))
         post
         (explainAfterLoop inv guards l)
  forM_ gcmds (structGdcmdInduct inv)
  tellPO (Conjunct [inv, Disjunct (map guardLoop guards)])
         (Bound (bnd `A.gte` A.Lit (A.Num 0) NoLoc) NoLoc)
         (explainTermination inv guards bnd l)
  forM_ gcmds (structGdcmdBnd inv bnd)
 struct (inv, Nothing) (A.Do gcmds l) post = do
  case fromLoc l of
    Nothing  -> return ()
    Just rng -> throwWarning (MissingBound rng)
  let guards = A.getGuards gcmds
  tellPO (Conjunct (inv : map (Negate . guardLoop) guards)) post (AtLoop l)
  forM_ gcmds (structGdcmdInduct inv)
 struct _        (A.Proof _ _)     _    = return ()
 -- TODO:
 struct _        A.Block{}         _    = return ()
 struct (pre, _) s@(A.Alloc _ _ l) post =
   tellPO' (AtAbort l) pre =<< wp s post
 struct (pre, _) s@(A.HLookup _ _ l) post =
   tellPO' (AtAbort l) pre =<< wp s post
 struct (pre, _) s@(A.HMutate _ _ l) post =
   tellPO' (AtAbort l) pre =<< wp s post
 struct (pre, _) s@(A.Dispose _ l) post =
   tellPO' (AtAbort l) pre =<< wp s post
 struct _        _                 _    = error "missing case in struct"

 structGdcmdInduct :: Pred -> A.GdCmd -> WP ()
 structGdcmdInduct inv (A.GdCmd guard body _) =
   structStmts Primary (Conjunct [inv, guardLoop guard], Nothing) body inv

 structGdcmdBnd :: Pred -> A.Expr -> A.GdCmd -> WP ()
 structGdcmdBnd inv bnd (A.GdCmd guard body _) = withFreshVar $ \oldbnd -> do
  -- oldbnd <- freshVar
  structStmts
    Secondary
    (Conjunct [inv, Bound (bnd `A.eqq` oldbnd) NoLoc, guardLoop guard], Nothing)
    body
    (Bound (bnd `A.lt` oldbnd) NoLoc)
