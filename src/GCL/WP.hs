{-# LANGUAGE OverloadedStrings, FlexibleContexts, DeriveGeneric, FlexibleInstances #-}


module GCL.WP where

import           Control.Monad.State     hiding ( guard )
import           Control.Monad.Writer    hiding ( guard )
import           Control.Monad.Except    hiding ( guard )

import qualified Data.Map                      as Map
import           Data.Loc                       ( Loc(..)
                                                , Located(..)
                                                )
import           Data.Aeson
import           GHC.Generics


import           Syntax.Abstract                ( Fresh(..) )
import           Syntax.Concrete                ( Expr
                                                , Stmt
                                                , Name
                                                , Defns
                                                )
import qualified Syntax.Concrete               as C
-- import qualified Syntax.Predicate as P
import           Syntax.Predicate        hiding ( Stmt )
import           Syntax.Location                ( Hydratable(..) )

import qualified GCL.Expr                      as E

type SM
  = WriterT [PO] (WriterT [Spec] (StateT (Int, Int, Int) (Either StructError)))

-- create a proof obligation
tellObli :: PO -> SM ()
tellObli obligation = tell [obligation]

obligate :: Pred -> Pred -> Origin -> SM ()
obligate p q l = unless (C.predEq (toExpr p) (toExpr q)) $ do
  -- NOTE: this could use some love
  (i, j, k) <- get
  put (succ i, j, k)
  tellObli $ PO i p q l

-- inform existence of a spec hole

tellSpec :: Pred -> Pred -> Loc -> SM ()
tellSpec p q loc = do
  (i, j, k) <- get
  put (i, succ j, k)
  lift $ tell [Specification j p q loc]

--------------------------------------------------------------------------------
-- | Structure, and Weakest-Precondition

struct :: Defns -> Bool -> Pred -> Maybe Expr -> Stmt -> Pred -> SM ()

struct _ _ pre _ (C.Abort l) _ = obligate pre (Constant C.false) (AtAbort l)

struct _ _ pre _ (C.Skip l) post = obligate pre post (AtSkip l)

struct _ _ pre _ (C.Assert p l) post = do
  obligate pre             (Assertion p l) (AtAssertion l)
  obligate (Assertion p l) post            (AtAssertion l)

struct _  _ _   _ (C.LoopInvariant _  _  l) _    = throwError (ExcessBound l)

struct ds _ pre _ (C.Assign        xs es l) post = do
  let denv = E.extendSubstWithDefns (assignmentEnv xs es) ds
  post' <- subst denv post
  obligate pre post' (AtAssignment l)

struct ds b pre _ (C.If gcmds l) post = do
  obligate pre (Disjunct $ map guardIf (C.getGuards gcmds)) (AtIf l)
  forM_ gcmds $ \(C.GdCmd guard body _) ->
    structStmts ds b (Conjunct [pre, guardIf guard]) Nothing body post

struct _  _ _   Nothing    (C.Do _     l) _    = throwError (MissingBound l)
 {- Or if we want to tolerate the user and carry on ---
 do -- warn that bnd is missing
  let gcmds' = map (\(GdCmd x y _) -> (depart x, y)) gcmds
  let guards = map fst gcmds'
  obligate (inv `A.conj` (A.conjunct (map A.neg guards))) post
  --
  forM_ gcmds' $ \(guard, body) -> do
    structStmts b (inv `A.conj` guard) Nothing body inv
 -}

struct ds b inv (Just bnd) (C.Do gcmds l) post = do
  -- base case
  let guards = C.getGuards gcmds
  obligate (Conjunct (inv : (map (Negate . guardLoop) guards))) post (AtLoop l)
  -- inductive cases
  forM_ gcmds $ \(C.GdCmd guard body _) ->
    structStmts ds b (Conjunct [inv, guardLoop guard]) Nothing body inv
  -- termination
  obligate (Conjunct (inv : map guardLoop guards))
           (Bound (bnd `C.gte` (C.Lit (C.Num 0) NoLoc)) NoLoc)
           (AtTermination l)
  -- bound decrementation
  oldbnd <- freshVar "bnd"
  forM_ gcmds $ \(C.GdCmd guard body _) -> structStmts
    ds
    False
    (Conjunct
      [ inv
      , Bound (bnd `C.eqq` C.Var (hydrate oldbnd) NoLoc) NoLoc
      , guardLoop guard
      ]
    )
    Nothing
    body
    (Bound (bnd `C.lt` C.Var (hydrate oldbnd) NoLoc) NoLoc)

struct _ _ _   _ (C.SpecQM l) _    = throwError $ DigHole l
struct _ b pre _ (C.Spec   l) post = when b (tellSpec pre post l)


structStmts :: Defns -> Bool -> Pred -> Maybe Expr -> [Stmt] -> Pred -> SM ()

structStmts _  _ pre _ []                     post = do 
  obligate pre post (AtAssertion (locOf pre))
  return ()

structStmts ds b pre _ (C.Assert p l : stmts) post = do
  obligate pre (Assertion p l) (AtAssertion l)
  structStmts ds b (Assertion p l) Nothing stmts post

structStmts ds b pre _ (C.LoopInvariant p bnd l : stmts) post = do
  let origin = if startsWithDo stmts then AtLoop l else AtAssertion l
  obligate pre (LoopInvariant p bnd l) origin
  structStmts ds b (LoopInvariant p bnd l) (Just bnd) stmts post

 where
  startsWithDo (C.Do _ _ : _) = True
  startsWithDo _              = False

structStmts ds b pre bnd (stmt : stmts) post = do
  post' <- wpStmts ds b stmts post
  struct ds b pre bnd stmt post'

data ProgView 
    = ProgViewEmpty 
    | ProgViewOkay Pred [Stmt] Pred
    | ProgViewMissingPrecondition [Stmt] Pred
    | ProgViewMissingPostcondition Pred [Stmt]
    | ProgViewMissingBoth [Stmt]

progView :: [Stmt] -> ProgView
progView [] = ProgViewEmpty
progView (C.Assert pre l:[]) = ProgViewMissingPrecondition [] (Assertion pre l)
progView stmts = case (head stmts, last stmts) of 
  (C.Assert pre l, C.Assert post m) -> ProgViewOkay (Assertion pre l) (init (tail stmts)) (Assertion post m)
  (C.Assert pre l, _) -> ProgViewMissingPostcondition (Assertion pre l) (tail stmts)
  (_, C.Assert post m) -> ProgViewMissingPrecondition (init stmts) (Assertion post m)
  (_, _) -> ProgViewMissingBoth stmts

structProg :: Defns -> [Stmt] -> SM ()
structProg defns statements = case progView statements of 
  ProgViewEmpty -> return ()
  ProgViewOkay pre stmts post -> structStmts defns True pre Nothing stmts post
  -- Missing precondition, insert { True } instead 
  ProgViewMissingPrecondition stmts post -> structStmts defns True (Constant C.true) Nothing stmts post
  ProgViewMissingPostcondition _pre stmts -> throwError (MissingPostcondition (locOf (last stmts)))
  ProgViewMissingBoth stmts -> throwError (MissingPostcondition (locOf (last stmts)))

wpStmts :: Defns -> Bool -> [Stmt] -> Pred -> SM Pred

wpStmts _ _ [] post = return post

wpStmts ds b (C.Assert pre l : stmts) post =
  structStmts ds b (Assertion pre l) Nothing stmts post
    >> return (Assertion pre l)

wpStmts ds b (C.LoopInvariant pre bnd l : stmts) post =
  structStmts ds b (LoopInvariant pre bnd l) (Just bnd) stmts post
    >> return (LoopInvariant pre bnd l)

wpStmts ds b (stmt : stmts) post = do
  post' <- wpStmts ds b stmts post
  wp ds b stmt post'

wp :: Defns -> Bool -> Stmt -> Pred -> SM Pred

wp _ _ (C.Abort _   ) _    = return (Constant C.false)

wp _ _ (C.Skip  _   ) post = return post

wp _ _ (C.Assert p l) post = do
  obligate (Assertion p l) post (AtAssertion l)
  return (Assertion p l)

wp _ _ (C.LoopInvariant p b l) post = do
  obligate (LoopInvariant p b l) post (AtAssertion l)
  return (LoopInvariant p b l)

wp ds _ (C.Assign xs es _) post = do
  let denv = E.extendSubstWithDefns (assignmentEnv xs es) ds
  subst denv post

wp ds b (C.If gcmds _) post = do
  forM_ gcmds $ \(C.GdCmd guard body _) ->
    structStmts ds b (guardIf guard) Nothing body post
  return (Disjunct (map guardIf (C.getGuards gcmds))) -- is this enough?

wp _ _ (C.Do _ l  ) _    = throwError (MissingAssertion l)

wp _ _ (C.SpecQM l) _    = throwError $ DigHole l

wp _ b (C.Spec   l) post = do
  when b (tellSpec post post l)
  return post  -- not quite right

assignmentEnv :: [Name] -> [Expr] -> C.Subst
assignmentEnv xs es = Map.fromList (zip (map C.nameToText xs) es)

--------------------------------------------------------------------------------
-- | The monad, and other supportive operations

instance Fresh SM where
  fresh = do
    (i, j, k) <- get
    put (i, j, succ k)
    return k

runSM
  :: SM a
  -> (Int, Int, Int)
  -> Either StructError (((a, [PO]), [Spec]), (Int, Int, Int))
runSM p = runStateT (runWriterT . runWriterT $ p)

runWP :: SM a -> Either StructError ((a, [PO]), [Spec])
runWP p = fmap fst $ runSM p (0, 0, 0)

censorSpec :: ([Spec] -> [Spec]) -> SM a -> SM a
censorSpec f = mapWriterT (censor f)


data StructError = MissingAssertion Loc
                 | MissingBound Loc
                 | ExcessBound  Loc
                 | MissingPostcondition Loc
                 | DigHole Loc
                deriving (Eq, Show, Generic)

instance Located StructError where
  locOf (MissingAssertion     loc) = loc
  locOf (MissingBound         loc) = loc
  locOf (ExcessBound          loc) = loc
  locOf (MissingPostcondition loc) = loc
  locOf (DigHole              loc) = loc

instance ToJSON StructError where
