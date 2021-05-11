{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}

module GCL.WP (sweep, StructError (..), StructWarning(..)) where

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

import GCL.Expr (Fresh (freshVar))
import qualified GCL.Expr as E
import GHC.Generics
import Syntax.Common (Name(..))
import Syntax.Abstract
  ( Defns,
    Expr,
    Stmt,
  )
import qualified Syntax.Abstract as A
import qualified Syntax.Abstract.Located as A
import Syntax.Predicate hiding (Stmt)
import Data.Loc.Range

type SM =
  ReaderT
    Defns
    ( WriterT
        [PO]
        ( WriterT
            [Spec]
            ( WriterT
                [StructWarning]
                (StateT (Int, Int, Int) (Either StructError))
            )
        )
    )

type SMSubst = ReaderT Int SM

-- create a proof obligation
tellObli :: PO -> SM ()
tellObli obligation = tell [obligation]

obligate :: Pred -> Pred -> Origin -> SM ()
obligate p q l = unless (A.predEq (toExpr p) (toExpr q)) $ do
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


throwWarning :: StructWarning -> SM ()
throwWarning warning = do 
  lift . lift . lift $ tell [warning]

--------------------------------------------------------------------------------

-- | Structure, and Weakest-Precondition
struct :: Bool -> Pred -> Maybe Expr -> Stmt -> Pred -> SM ()
struct _ pre _ (A.Abort l) _ = obligate pre (Constant A.false) (AtAbort l)
struct _ pre _ (A.Skip l) post = obligate pre post (AtSkip l)
struct True pre _ (A.Assert p l) post = do
  obligate pre (Assertion p l) (AtAssertion l)
  obligate (Assertion p l) post (AtAssertion l)
struct False pre _ (A.Assert _ l) post = do
  obligate pre post (AtAssertion l)
struct _ _ _ (A.LoopInvariant _ _ l) _ = case fromLoc l of 
  Nothing -> return ()
  Just range -> throwWarning $ ExcessBound range
struct _ pre _ (A.Assign xs es l) post = do
  let denv = assignmentEnv xs es -- E.extendSubstWithDefns (assignmentEnv xs es) ds
  post' <- runReaderT (subst denv post :: SMSubst Pred) 0
  obligate pre post' (AtAssignment l)
struct True pre _ (A.If gcmds l) post = do
  obligate pre (Disjunct $ map guardIf (A.getGuards gcmds)) (AtIf l)
  forM_ gcmds $ \(A.GdCmd guard body _) ->
    structStmts True (Conjunct [pre, guardIf guard]) Nothing body post
struct False pre _ (A.If gcmds _) post = do
  forM_ gcmds $ \(A.GdCmd guard body _) ->
    structStmts False (Conjunct [pre, guardIf guard]) Nothing body post
struct _ inv Nothing (A.Do gcmds l) post = do
  do
    -- warning user about missing "bnd"
    case fromLoc l of 
      Nothing -> return ()
      Just range -> throwWarning (MissingBound range)
    -- base case
    let guards = A.getGuards gcmds
    obligate (Conjunct (inv : map (Negate . guardLoop) guards)) post (AtLoop l)
    -- inductive cases
    forM_ gcmds $ \(A.GdCmd guard body _) ->
      structStmts True (Conjunct [inv, guardLoop guard]) Nothing body inv
    -- termination
    obligate
      (Conjunct (inv : map guardLoop guards))
      post
      (AtTermination l)
-- Issue: #4
{- Or if we want to tolerate the user and carry on ---
do -- warn that bnd is missing
 let gcmds' = map (\(GdCmd x y _) -> (depart x, y)) gcmds
 let guards = map fst gcmds'
 obligate (inv `A.conj` (A.conjunct (map A.neg guards))) post
 --
 forM_ gcmds' $ \(guard, body) -> do
   structStmts b (inv `A.conj` guard) Nothing body inv
-}

struct True inv (Just bnd) (A.Do gcmds l) post = do
  -- base case
  let guards = A.getGuards gcmds
  obligate (Conjunct (inv : map (Negate . guardLoop) guards)) post (AtLoop l)
  -- inductive cases
  forM_ gcmds $ \(A.GdCmd guard body _) ->
    structStmts True (Conjunct [inv, guardLoop guard]) Nothing body inv
  -- termination
  obligate
    (Conjunct (inv : map guardLoop guards))
    (Bound (bnd `A.gte` A.Lit (A.Num 0) NoLoc) NoLoc)
    (AtTermination l)
  -- bound decrementation
  oldbnd <- freshVar "bnd"
  forM_ gcmds $ \(A.GdCmd guard body _) ->
    structStmts
      False
      ( Conjunct
          [ inv,
            Bound (bnd `A.eqq` A.Var (Name oldbnd NoLoc) NoLoc) NoLoc,
            guardLoop guard
          ]
      )
      Nothing
      body
      (Bound (bnd `A.lt` A.Var (Name oldbnd NoLoc) NoLoc) NoLoc)
struct False inv _ (A.Do gcmds l) post = do
  let guards = A.getGuards gcmds
  obligate (Conjunct (inv : map (Negate . guardLoop) guards)) post (AtLoop l)
struct b pre _ (A.Spec _ l) post = when b (tellSpec pre post l)
-- banacorn: what to do about this?
struct _ _ _ (A.Proof _) _ = return ()

structStmts :: Bool -> Pred -> Maybe Expr -> [Stmt] -> Pred -> SM ()
structStmts _ pre _ [] post = do
  -- the precondition may be a Constant and have no srcloc
  -- in that case, use the srcloc of postcondition instead
  case locOf pre of 
    NoLoc -> obligate pre post (AtAssertion (locOf post))
    others -> obligate pre post (AtAssertion others)
  return ()
structStmts True pre _ (A.Assert p l : stmts) post = do
  obligate pre (Assertion p l) (AtAssertion l)
  structStmts True (Assertion p l) Nothing stmts post
structStmts False pre _ (A.Assert _ _ : stmts) post = do
  structStmts False pre Nothing stmts post
structStmts True pre _ (A.LoopInvariant p bnd l : stmts) post = do
  let origin = if startsWithDo stmts then AtLoop l else AtAssertion l
  obligate pre (LoopInvariant p bnd l) origin
  structStmts True (LoopInvariant p bnd l) (Just bnd) stmts post

-- SCM: think about this one later
structStmts False pre _ (A.LoopInvariant {} : stmts) post = do
  structStmts False pre Nothing stmts post
structStmts b pre bnd (stmt : stmts) post = do
  post' <- wpStmts b stmts post
  struct b pre bnd stmt post'

startsWithDo :: [Stmt] -> Bool
startsWithDo (A.Do _ _ : _) = True
startsWithDo _ = False

data ProgView
  = ProgViewEmpty
  | ProgViewOkay Pred [Stmt] Pred
  | ProgViewMissingPrecondition [Stmt] Pred
  | ProgViewMissingPostcondition Pred [Stmt]
  | ProgViewMissingBoth [Stmt]

progView :: [Stmt] -> ProgView
progView [] = ProgViewEmpty
progView [A.Assert pre l] = ProgViewMissingPrecondition [] (Assertion pre l)
progView stmts = case (head stmts, last stmts) of
  (A.Assert pre l, A.Assert post m) -> ProgViewOkay (Assertion pre l) (init (tail stmts)) (Assertion post m)
  (A.Assert pre l, _) -> ProgViewMissingPostcondition (Assertion pre l) (tail stmts)
  (_, A.Assert post m) -> ProgViewMissingPrecondition (init stmts) (Assertion post m)
  (_, _) -> ProgViewMissingBoth stmts

structProg :: [Stmt] -> SM ()
structProg statements = case progView statements of
  ProgViewEmpty -> return ()
  ProgViewOkay pre stmts post -> structStmts True pre Nothing stmts post
  -- Missing precondition, insert { True } instead
  ProgViewMissingPrecondition stmts post -> structStmts True (Constant A.true) Nothing stmts post
  ProgViewMissingPostcondition _pre stmts -> throwError (MissingPostcondition (locOf (last stmts)))
  ProgViewMissingBoth stmts -> throwError (MissingPostcondition (locOf (last stmts)))

wpStmts :: Bool -> [Stmt] -> Pred -> SM Pred
wpStmts _ [] post = return post
wpStmts True (A.Assert pre l : stmts) post =
  structStmts True (Assertion pre l) Nothing stmts post
    >> return (Assertion pre l)
wpStmts False (A.Assert _ _ : stmts) post =
  wpStmts False stmts post
wpStmts True (A.LoopInvariant pre bnd l : stmts) post =
  structStmts True (LoopInvariant pre bnd l) (Just bnd) stmts post
    >> return (LoopInvariant pre bnd l)
wpStmts False (A.LoopInvariant {} : stmts) post =
  wpStmts False stmts post
wpStmts b (stmt : stmts) post = do
  post' <- wpStmts b stmts post
  wp b stmt post'

wp :: Bool -> Stmt -> Pred -> SM Pred
wp _ (A.Abort _) _ = return (Constant A.false)
wp _ (A.Skip _) post = return post
wp _ (A.Assert p l) post = do
  obligate (Assertion p l) post (AtAssertion l)
  return (Assertion p l)
wp _ (A.LoopInvariant p b l) post = do
  obligate (LoopInvariant p b l) post (AtAssertion l)
  return (LoopInvariant p b l)
wp _ (A.Assign xs es _) post = do
  let denv = assignmentEnv xs es -- E.extendSubstWithDefns (assignmentEnv xs es) ds
  runReaderT (subst denv post :: SMSubst Pred) 0
wp b (A.If gcmds _) post = do
  -- forM_ gcmds $ \(A.GdCmd guard body _) ->
  --   structStmts b (guardIf guard) Nothing body post
  -- return (Disjunct (map guardIf (A.getGuards gcmds))) -- is this enough?
  let disGuards = disjunct (map guardIf (A.getGuards gcmds))
  pres <- forM gcmds $ \(A.GdCmd guard body _) ->
    Constant . (guard `A.imply`) . toExpr
      <$> wpStmts b body post
  return (conjunct (disGuards : pres))
wp _ (A.Do _ l) _ = throwError (MissingAssertion l)
wp b (A.Spec _ l) post = do
  when b (tellSpec post post l)
  return post -- not quite right
wp _ (A.Proof _) post = do
  return post -- banacorn: not sure if this is right

assignmentEnv :: [Name] -> [Expr] -> A.Subst
assignmentEnv xs es = Map.fromList (zip xs es)

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
  Either StructError (((a, [PO]), [Spec]), [StructWarning])
runSM p defs = evalStateT (runWriterT . runWriterT . runWriterT $ runReaderT p defs) (0, 0, 0)

sweep :: A.Program -> Either StructError ([PO], [Spec], [StructWarning])
sweep (A.Program _ _ ds statements _) = do
  (((_, pos'), specs), warnings) <- runSM (structProg statements) ds
  return (pos', specs, warnings)

data StructWarning
  = MissingBound Range
  | ExcessBound Range
  deriving (Eq, Show, Generic)

instance Located StructWarning where
  locOf (MissingBound range) = locOf range
  locOf (ExcessBound range) = locOf range

data StructError
  = MissingAssertion Loc
  | MissingPostcondition Loc
  deriving (Eq, Show, Generic)

instance Located StructError where
  locOf (MissingAssertion loc) = loc
  locOf (MissingPostcondition loc) = loc

instance ToJSON StructError
