{-# LANGUAGE OverloadedStrings #-}

module GCL.WP.SP where

import           Control.Arrow                  ( first )
import           Control.Monad.Except           ( MonadError(throwError)
                                                , forM )
import           Data.Loc                       ( Loc(..) )
import           GCL.Predicate                  ( Pred )
import           GCL.Common
import           GCL.Substitution               ( syntaxSubst )
import           GCL.WP.Types
import           GCL.WP.Util
import           Syntax.Typed
import           Syntax.Typed.Operator          ( true
                                                , eqq
                                                , nameVar
                                                , neg
                                                , disjunct
                                                , conj
                                                , conjunct
                                                , exists )
import           Syntax.Typed.Util              ( getGuards
                                                , typeOf )
import           Syntax.Common                  ( nameToText )

spFunctions :: (TstructSegs, Tstruct) -> TspSStmts
spFunctions (structSegs, struct) = spSStmts
 where
 spStmts :: (Pred, Maybe Expr) -> [Stmt] -> WP Pred
 spStmts (pre, bnd) = spSegs (pre, bnd) . groupStmts

 spSegs :: (Pred, Maybe Expr) -> [SegElm] -> WP Pred
 spSegs (pre, bnd) segs = case split segs of
   (ls, Nothing                        ) -> spSegs' (pre, bnd) ls
   (ls, Just (SAsrt (Assert p _), rs)) -> do
     structSegs (pre, bnd) ls p
     spSegs' (p, Nothing) rs
   (ls, Just (SAsrt (LoopInvariant p bd _), rs)) -> do
     structSegs (pre, bnd) ls p
     spSegs' (p, Just bd) rs
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
   spSegs' :: (Pred, Maybe Expr) -> [SegElm] -> WP Pred
   spSegs' (pre', _   ) []                  = return pre'
   spSegs' (pre', bnd') (SStmts ss : segs') = do
     pre'' <- spSStmts (pre', bnd') ss
     spSegs' (pre'', Nothing) segs'
   spSegs' (pre', bnd') (SSpec (Spec _ range tenv) : segs') = do
     tellSpec pre' pre' tenv range
     spSegs' (pre', bnd') segs'
   spSegs' _ _ = error "missing case in spSegs'"

  -- the "simple" version
 spSStmts :: (Pred, Maybe Expr) -> [Stmt] -> WP Pred
 spSStmts (pre, _  ) []             = return pre
 spSStmts (pre, bnd) (stmt : stmts) = do
  pre' <- sp (pre, bnd) stmt
  spSStmts (pre', Nothing) stmts

 sp :: (Pred, Maybe Expr) -> Stmt -> WP Pred
 sp (_  , _) (Abort _       ) = return true

 sp (pre, _) (Skip  _       ) = return pre

 sp (pre, _) (Assign xs es _) = do
      -- {P} x := E { (exists x' :: x = E[x'/x] && P[x'/x]) }
  -- generate fresh names from the assignees "xs"
  freNames <- freshNames (map nameToText xs)
  let freVars = zipWith nameVar freNames (map typeOf es)
  -- substitute "xs"s with fresh names in "pre"
  let pre' = syntaxSubst xs freVars pre

  let pairs = zip freVars es
  let predicates = [x `eqq` syntaxSubst xs freVars e  | (x, e) <- pairs]
  return $ exists freNames (conjunct predicates) pre'

 sp (pre, _) (AAssign (Var x t _) i e _) = do
  -- {P} x[I] := E { (exist x' :: x = x'[I[x'/x] -> E[x'/x]] && P[x'/x]) }
  xn   <- freshName' (nameToText x)
  let x' = nameVar xn t
  let pre' = syntaxSubst [x] [x'] pre
  let i'   = syntaxSubst [x] [x'] i
  let e'   = syntaxSubst [x] [x'] e
  return $ exists [xn]
              (nameVar x t `eqq` ArrUpd x' i' e' NoLoc)
              pre'

 sp (_  , _) (AAssign _ _ _ l) = throwError (MultiDimArrayAsgnNotImp l)

 sp (pre, _) (If gcmds _     ) = do
  posts <- forM gcmds $ \(GdCmd guard body _) ->
    spStmts (guard `conj` pre, Nothing) body
  return (disjunct posts)

 sp (pre, bnd) (Do gcmds l) = do
  let guards = getGuards gcmds
  let post = conjunct (pre : map neg guards)
  struct (pre, bnd) (Do gcmds l) post
  return post

 sp (pre, _) _ = return pre
