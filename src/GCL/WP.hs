{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}

module GCL.WP where

import Control.Monad.Except hiding (guard)
import Control.Monad.Reader hiding (guard)
import Control.Monad.State hiding (guard)
import Control.Monad.Writer hiding (guard)
import Data.Aeson
import Data.Loc
  ( Loc (..),
    Located (..),
  )
import qualified Data.Map as Map
-- import qualified Syntax.Predicate as P

import qualified GCL.Expr as E
import GHC.Generics
import Syntax.Concrete
  ( Defns,
    Expr,
    Name,
    Stmt,
  )
import qualified Syntax.Concrete as C
import Syntax.Predicate hiding (Stmt)
import GCL.Expr (Fresh(freshVar))

type SM =
  ReaderT
    Defns
    ( WriterT
        [PO]
        ( WriterT
            [Spec]
            (StateT (Int, Int, Int) (Either StructError))
        )
    )

type SMSubst = ReaderT Int SM

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
  lift . lift $ tell [Specification j p q loc]

--------------------------------------------------------------------------------

-- | Structure, and Weakest-Precondition
struct :: Bool -> Pred -> Maybe Expr -> Stmt -> Pred -> SM ()
struct _ pre _ (C.Abort l) _ = obligate pre (Constant C.false) (AtAbort l)
struct _ pre _ (C.Skip l) post = obligate pre post (AtSkip l)
struct True pre _ (C.Assert p l) post = do
  obligate pre (Assertion p l) (AtAssertion l)
  obligate (Assertion p l) post (AtAssertion l)
struct False pre _ (C.Assert _ l) post = do
  obligate pre post (AtAssertion l)
struct _ _ _ (C.LoopInvariant _ _ l) _ = throwError (ExcessBound l)
struct _ pre _ (C.Assign xs es l) post = do
  let denv = assignmentEnv xs es -- E.extendSubstWithDefns (assignmentEnv xs es) ds
  post' <- runReaderT (subst denv post :: SMSubst Pred) 0
  obligate pre post' (AtAssignment l)
struct True pre _ (C.If gcmds l) post = do
  obligate pre (Disjunct $ map guardIf (C.getGuards gcmds)) (AtIf l)
  forM_ gcmds $ \(C.GdCmd guard body _) ->
    structStmts True (Conjunct [pre, guardIf guard]) Nothing body post
struct False pre _ (C.If gcmds _) post = do
  forM_ gcmds $ \(C.GdCmd guard body _) ->
    structStmts False (Conjunct [pre, guardIf guard]) Nothing body post
struct _ _ Nothing (C.Do _ l) _ = throwError (MissingBound l)
{- Or if we want to tolerate the user and carry on ---
do -- warn that bnd is missing
 let gcmds' = map (\(GdCmd x y _) -> (depart x, y)) gcmds
 let guards = map fst gcmds'
 obligate (inv `A.conj` (A.conjunct (map A.neg guards))) post
 --
 forM_ gcmds' $ \(guard, body) -> do
   structStmts b (inv `A.conj` guard) Nothing body inv
-}

struct True inv (Just bnd) (C.Do gcmds l) post = do
  -- base case
  let guards = C.getGuards gcmds
  obligate (Conjunct (inv : (map (Negate . guardLoop) guards))) post (AtLoop l)
  -- inductive cases
  forM_ gcmds $ \(C.GdCmd guard body _) ->
    structStmts True (Conjunct [inv, guardLoop guard]) Nothing body inv
  -- termination
  obligate
    (Conjunct (inv : map guardLoop guards))
    (Bound (bnd `C.gte` (C.Lit (C.Num 0) NoLoc)) NoLoc)
    (AtTermination l)
  -- bound decrementation
  oldbnd <- freshVar "bnd"
  forM_ gcmds $ \(C.GdCmd guard body _) ->
    structStmts
      False
      ( Conjunct
          [ inv,
            Bound (bnd `C.eqq` C.Var (C.Name oldbnd NoLoc) NoLoc) NoLoc,
            guardLoop guard
          ]
      )
      Nothing
      body
      (Bound (bnd `C.lt` C.Var (C.Name oldbnd NoLoc) NoLoc) NoLoc)
struct False inv _ (C.Do gcmds l) post = do
  let guards = C.getGuards gcmds
  obligate (Conjunct (inv : (map (Negate . guardLoop) guards))) post (AtLoop l)
struct _ _ _ (C.SpecQM l) _ = throwError $ DigHole l
struct b pre _ (C.Spec l) post = when b (tellSpec pre post l)
-- banacorn: what to do about this?
struct _ _ _ (C.Proof _) _ = return ()

structStmts :: Bool -> Pred -> Maybe Expr -> [Stmt] -> Pred -> SM ()
structStmts _ pre _ [] post = do
  obligate pre post (AtAssertion (locOf pre))
  return ()
structStmts True pre _ (C.Assert p l : stmts) post = do
  obligate pre (Assertion p l) (AtAssertion l)
  structStmts True (Assertion p l) Nothing stmts post
structStmts False pre _ (C.Assert _ _ : stmts) post = do
  structStmts False pre Nothing stmts post
structStmts True pre _ (C.LoopInvariant p bnd l : stmts) post = do
  let origin = if startsWithDo stmts then AtLoop l else AtAssertion l
  obligate pre (LoopInvariant p bnd l) origin
  structStmts True (LoopInvariant p bnd l) (Just bnd) stmts post

-- SCM: think about this one later
structStmts False pre _ (C.LoopInvariant _ _ _ : stmts) post = do
  structStmts False pre Nothing stmts post
structStmts b pre bnd (stmt : stmts) post = do
  post' <- wpStmts b stmts post
  struct b pre bnd stmt post'

startsWithDo :: [Stmt] -> Bool
startsWithDo (C.Do _ _ : _) = True
startsWithDo _ = False

data ProgView
  = ProgViewEmpty
  | ProgViewOkay Pred [Stmt] Pred
  | ProgViewMissingPrecondition [Stmt] Pred
  | ProgViewMissingPostcondition Pred [Stmt]
  | ProgViewMissingBoth [Stmt]

progView :: [Stmt] -> ProgView
progView [] = ProgViewEmpty
progView (C.Assert pre l : []) = ProgViewMissingPrecondition [] (Assertion pre l)
progView stmts = case (head stmts, last stmts) of
  (C.Assert pre l, C.Assert post m) -> ProgViewOkay (Assertion pre l) (init (tail stmts)) (Assertion post m)
  (C.Assert pre l, _) -> ProgViewMissingPostcondition (Assertion pre l) (tail stmts)
  (_, C.Assert post m) -> ProgViewMissingPrecondition (init stmts) (Assertion post m)
  (_, _) -> ProgViewMissingBoth stmts

structProg :: [Stmt] -> SM ()
structProg statements = case progView statements of
  ProgViewEmpty -> return ()
  ProgViewOkay pre stmts post -> structStmts True pre Nothing stmts post
  -- Missing precondition, insert { True } instead
  ProgViewMissingPrecondition stmts post -> structStmts True (Constant C.true) Nothing stmts post
  ProgViewMissingPostcondition _pre stmts -> throwError (MissingPostcondition (locOf (last stmts)))
  ProgViewMissingBoth stmts -> throwError (MissingPostcondition (locOf (last stmts)))

wpStmts :: Bool -> [Stmt] -> Pred -> SM Pred
wpStmts _ [] post = return post
wpStmts True (C.Assert pre l : stmts) post =
  structStmts True (Assertion pre l) Nothing stmts post
    >> return (Assertion pre l)
wpStmts False (C.Assert _ _ : stmts) post =
  wpStmts False stmts post
wpStmts True (C.LoopInvariant pre bnd l : stmts) post =
  structStmts True (LoopInvariant pre bnd l) (Just bnd) stmts post
    >> return (LoopInvariant pre bnd l)
wpStmts False (C.LoopInvariant _ _ _ : stmts) post =
  wpStmts False stmts post
wpStmts b (stmt : stmts) post = do
  post' <- wpStmts b stmts post
  wp b stmt post'

wp :: Bool -> Stmt -> Pred -> SM Pred
wp _ (C.Abort _) _ = return (Constant C.false)
wp _ (C.Skip _) post = return post
wp _ (C.Assert p l) post = do
  obligate (Assertion p l) post (AtAssertion l)
  return (Assertion p l)
wp _ (C.LoopInvariant p b l) post = do
  obligate (LoopInvariant p b l) post (AtAssertion l)
  return (LoopInvariant p b l)
wp _ (C.Assign xs es _) post = do
  let denv = assignmentEnv xs es -- E.extendSubstWithDefns (assignmentEnv xs es) ds
  runReaderT (subst denv post :: SMSubst Pred) 0
wp b (C.If gcmds _) post = do
  -- forM_ gcmds $ \(C.GdCmd guard body _) ->
  --   structStmts b (guardIf guard) Nothing body post
  -- return (Disjunct (map guardIf (C.getGuards gcmds))) -- is this enough?
  let disGuards = disjunct (map guardIf (C.getGuards gcmds))
  pres <- forM gcmds $ \(C.GdCmd guard body _) ->
    (Constant . (guard `C.imply`) . toExpr)
      <$> wpStmts b body post
  return (conjunct (disGuards : pres))
wp _ (C.Do _ l) _ = throwError (MissingAssertion l)
wp _ (C.SpecQM l) _ = throwError $ DigHole l
wp b (C.Spec l) post = do
  when b (tellSpec post post l)
  return post -- not quite right
wp _ (C.Proof _) post = do
  return post -- banacorn: not sure if this is right

assignmentEnv :: [Name] -> [Expr] -> C.Subst
assignmentEnv xs es = Map.fromList (zip (map C.nameToText xs) es)

--------------------------------------------------------------------------------

-- | The monad, and other supportive operations
instance Fresh SM where
  fresh = do
    (i, j, k) <- get
    put (i, j, succ k)
    return k

instance E.DefsM SM where
  askDefns = ask

instance Fresh SMSubst where
  fresh = do
    (i, j, k) <- get
    put (i, j, succ k)
    return k

instance E.DefsM SMSubst where
  askDefns = lift ask

instance E.ExpandM SMSubst where
  ifExpand me mb = do
    i <- ask
    if i == 0
      then mb
      else local (\n -> n - 1) me
  doExLevel n = local (const n)

runSM ::
  SM a ->
  Defns ->
  (Int, Int, Int) ->
  Either StructError (((a, [PO]), [Spec]), (Int, Int, Int))
runSM p defs = runStateT (runWriterT . runWriterT $ runReaderT p defs)

runWP :: SM a -> Defns -> Either StructError ((a, [PO]), [Spec])
runWP p defs = fmap fst $ runSM p defs (0, 0, 0)

-- censorSpec :: ([Spec] -> [Spec]) -> SM a -> SM a
-- censorSpec f = mapWriterT (censor f)

data StructError
  = MissingAssertion Loc
  | MissingBound Loc
  | ExcessBound Loc
  | MissingPostcondition Loc
  | DigHole Loc
  deriving (Eq, Show, Generic)

instance Located StructError where
  locOf (MissingAssertion loc) = loc
  locOf (MissingBound loc) = loc
  locOf (ExcessBound loc) = loc
  locOf (MissingPostcondition loc) = loc
  locOf (DigHole loc) = loc

instance ToJSON StructError
