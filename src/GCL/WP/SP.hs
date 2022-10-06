{-# LANGUAGE OverloadedStrings #-}

module GCL.WP.SP where

import           Control.Arrow                  ( first )
import           Control.Monad.Except           ( MonadError(throwError)
                                                , forM )
import           Data.Loc                       ( Loc(..) )
import           GCL.Predicate                  ( Pred(..) )
import           GCL.Predicate.Util             ( disjunct
                                                , guardLoop
                                                , toExpr
                                                )
import           GCL.Common                     ( freshWithLabel
                                                , freshText
                                                )
import           Pretty                         ( toText )
import GCL.WP.Type
import GCL.WP.Util
import qualified Syntax.Abstract               as A
import qualified Syntax.Abstract.Operator      as A
import qualified Syntax.Abstract.Util          as A
import           Syntax.Common                  ( Name(Name) )

spFunctions :: (TstructSegs, Tstruct) -> TspSStmts
spFunctions (structSegs, struct) = spSStmts
 where
 spStmts :: (Pred, Maybe A.Expr) -> [A.Stmt] -> WP Pred
 spStmts (pre, bnd) = spSegs (pre, bnd) . groupStmts

 spSegs :: (Pred, Maybe A.Expr) -> [SegElm] -> WP Pred
 spSegs (pre, bnd) segs = case split segs of
   (ls, Nothing                        ) -> spSegs' (pre, bnd) ls
   (ls, Just (SAsrt (A.Assert p l), rs)) -> do
     structSegs (pre, bnd) ls (Assertion p l)
     spSegs' (Assertion p l, Nothing) rs
   (ls, Just (SAsrt (A.LoopInvariant p bd l), rs)) -> do
     structSegs (pre, bnd) ls (Assertion p l)
     spSegs' (Assertion p l, Just bd) rs
   (_, _) -> error "missing case in spSegs"
  where
   split :: [SegElm] -> ([SegElm], Maybe (SegElm, [SegElm]))
   split []                     = ([], Nothing)
   split (s@(SStmts _) : segs') = first (s :) (split segs')
   split (s@(SSpec  _) : segs') = first (s :) (split segs')
   split (s@(SAsrt  _) : segs') = case split segs' of
    (ls, Nothing   ) -> ([], Just (s, ls))
    (ls, r@(Just _)) -> (s : ls, r)

   -- spSeg' deals with a block with no assertions
   spSegs' :: (Pred, Maybe A.Expr) -> [SegElm] -> WP Pred
   spSegs' (pre', _   ) []                  = return pre'
   spSegs' (pre', bnd') (SStmts ss : segs') = do
     pre'' <- spSStmts (pre', bnd') ss
     spSegs' (pre'', Nothing) segs'
   spSegs' (pre', bnd') (SSpec (A.Spec _ range) : segs') = do
     tellSpec pre' pre' range
     spSegs' (pre', bnd') segs'
   spSegs' _ _ = error "missing case in spSegs'"

  -- the "simple" version
 spSStmts :: (Pred, Maybe A.Expr) -> [A.Stmt] -> WP Pred
 spSStmts (pre, _  ) []             = return pre
 spSStmts (pre, bnd) (stmt : stmts) = do
  pre' <- sp (pre, bnd) stmt
  spSStmts (pre', Nothing) stmts

 sp :: (Pred, Maybe A.Expr) -> A.Stmt -> WP Pred
 sp (_  , _) (A.Abort _       ) = return (Constant A.true)

 sp (pre, _) (A.Skip  _       ) = return pre

 sp (pre, _) (A.Assign xs es l) = do
      -- {P} x := E { (exists x' :: x = E[x'/x] && P[x'/x]) }
  -- generate fresh names from the assignees "xs"
  freshNames <- forM xs $ \x -> do
    x' <- freshWithLabel (toText x)
    return $ Name x' NoLoc
  let freshVars = map (`A.Var` l) freshNames
  -- substitute "xs"s with fresh names in "pre"
  pre' <- substitute xs freshVars (toExpr pre)


  --
  let pairs = zip xs es
  predicates <- forM pairs $ \(x, e) -> do
    e' <- substitute xs freshVars e
    return $ A.nameVar x `A.eqq` e'
  return $ Constant (A.exists freshNames (A.conjunct predicates) pre')

 sp (pre, _) (A.AAssign (A.Var x _) i e _) = do
  -- {P} x[I] := E { (exist x' :: x = x'[I[x'/x] -> E[x'/x]] && P[x'/x]) }
  x'   <- freshText

  pre' <- substitute [x] [A.variable x'] (toExpr pre)
  i'   <- substitute [x] [A.variable x'] i
  e'   <- substitute [x] [A.variable x'] e

  return $ Constant
    (A.exists [Name x' NoLoc]
              (A.nameVar x `A.eqq` A.ArrUpd (A.variable x') i' e' NoLoc)
              pre'
    )

 sp (_  , _) (A.AAssign _ _ _ l) = throwError (MultiDimArrayAsgnNotImp l)

 sp (pre, _) (A.If gcmds _     ) = do
  posts <- forM gcmds $ \(A.GdCmd guard body _) ->
    Constant
      .   toExpr
      <$> spStmts (Constant (guard `A.conj` toExpr pre), Nothing) body
  return (disjunct posts)
 sp (pre, bnd) (A.Do gcmds l) = do
  let guards = A.getGuards gcmds
  let post = Conjunct (pre : map (Negate . guardLoop) guards)
  struct (pre, bnd) (A.Do gcmds l) post
  return post
 sp (pre, _) _ = return pre
