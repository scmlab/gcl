{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}

module GCL.WP where

import Control.Monad.Except (Except, MonadError (throwError), forM, forM_, runExcept, unless, when)
import Control.Monad.RWS (MonadReader (ask), MonadState (..), MonadWriter (..), RWST, evalRWST)
import Data.Aeson (ToJSON)
import Data.Loc (Loc (..), Located (..))
import Data.Loc.Range (Range, fromLoc)
import qualified Data.Map as Map
import GCL.Common (Bindings, Fresh (fresh, freshText), Subs, Substitutable (subst))
import GCL.Predicate (Origin (..), PO (..), Pred (..), Spec (Specification))
import GCL.Predicate.Util (conjunct, disjunct, guardIf, guardLoop, toExpr)
import GHC.Generics (Generic)
import qualified Syntax.Abstract as A
import qualified Syntax.Abstract.Operator as A
import qualified Syntax.Abstract.Util as A
import Syntax.Common (Name (Name))

type TM = Except StructError

type WP = RWST (Subs A.Expr) ([PO], [Spec], [StructWarning]) (Int, Int, Int) TM

instance Fresh WP where
  fresh = do
    (i, j, k) <- get
    put (i, j, succ k)
    return k

runWP :: WP a -> Subs A.Expr -> Either StructError (a, ([PO], [Spec], [StructWarning]))
runWP p defs = runExcept $ evalRWST p defs (0, 0, 0)

sweep :: A.Program -> Either StructError ([PO], [Spec], [StructWarning])
sweep (A.Program _ _ ds stmts _) = do
  snd <$> runWP (structProgram stmts) ds

data ProgView
  = ProgViewEmpty
  | ProgViewOkay Pred [A.Stmt] Pred
  | ProgViewMissingPrecondition [A.Stmt] Pred
  | ProgViewMissingPostcondition Pred [A.Stmt]
  | ProgViewMissingBoth [A.Stmt]

progView :: [A.Stmt] -> ProgView
progView [] = ProgViewEmpty
progView [A.Assert pre l] = do
  ProgViewMissingPrecondition [] (Assertion pre l)
progView stmts = do
  case (head stmts, last stmts) of
    (A.Assert pre l, A.Assert post m) -> do
      ProgViewOkay (Assertion pre l) (init (tail stmts)) (Assertion post m)
    (A.Assert pre l, _) -> do
      ProgViewMissingPostcondition (Assertion pre l) (tail stmts)
    (_, A.Assert post m) -> do
      ProgViewMissingPrecondition (init stmts) (Assertion post m)
    _ -> ProgViewMissingBoth stmts

structProgram :: [A.Stmt] -> WP ()
structProgram stmts = do
  env <- Map.map Right <$> ask :: WP (Subs Bindings)

  case progView (subst env stmts) of
    ProgViewEmpty -> return ()
    ProgViewOkay pre stmts' post -> structStmts True (pre,Nothing) stmts' post
    ProgViewMissingPrecondition stmts' post -> structStmts True (Constant A.true, Nothing) stmts' post
    ProgViewMissingPostcondition _ stmts' -> throwError . MissingPostcondition . locOf . last $ stmts'
    ProgViewMissingBoth stmts' -> throwError . MissingPostcondition . locOf . last $ stmts'


data SegElm = SAsrt A.Stmt
            | SSpec A.Stmt
            | SStmts [A.Stmt]

groupStmts :: [A.Stmt] -> [SegElm]
groupStmts [] = []
groupStmts (s@(A.Assert _ _) : stmts) = SAsrt s : groupStmts stmts
groupStmts (s@(A.LoopInvariant _ _ _) : stmts) = SAsrt s : groupStmts stmts
groupStmts (s@(A.Spec _ _) : stmts) = SSpec s : groupStmts stmts
groupStmts (s : stmts) = case groupStmts stmts of
    [] -> [SStmts [s]]
    (SStmts ss : segs) -> SStmts (s:ss) : segs
    (s' : segs) -> SStmts [s] : s' : segs

structStmts :: Bool -> (Pred, Maybe A.Expr) -> [A.Stmt] -> Pred -> WP ()
structStmts b pre stmts post =
  structSegs b pre (groupStmts stmts) post

structSegs :: Bool -> (Pred, Maybe A.Expr) -> [SegElm] -> Pred -> WP ()
structSegs _ (pre, _) [] post = do
  case locOf pre of
    NoLoc -> tellPO pre post (AtAssertion (locOf post))
    others -> tellPO pre post (AtAssertion others)
  return ()
structSegs True (pre, _) (SAsrt (A.Assert p l) : segs) post = do
    let assert = Assertion p l
    tellPO pre assert (AtAssertion (locOf pre))
    structSegs True (assert, Nothing) segs post
structSegs False (pre, bnd) (SAsrt (A.Assert _ _) : segs) post =
  structSegs False (pre, bnd) segs post
structSegs True (pre, _) (SAsrt (A.LoopInvariant p bnd l) : segs) post = do
  let loopInv = LoopInvariant p bnd l
  tellPO pre loopInv origin
  structSegs True (loopInv, Just bnd) segs post
 where startsWithDo :: [SegElm] -> Bool
       startsWithDo (SStmts (A.Do _ _ : _) : _) = True
       startsWithDo _ = False
       origin = if startsWithDo segs then AtLoop l else AtAssertion l
structSegs False (pre, bnd) (SAsrt (A.LoopInvariant {}) : segs) post =
  structSegs False (pre, bnd) segs post
structSegs b (pre, bnd) [SStmts ss] post =
  structSStmts b (pre,bnd) ss post
structSegs b (pre, bnd) (SStmts ss : SAsrt (A.Assert p l) : segs) post = do
  structSStmts b (pre, bnd) ss (Assertion p l)
  structSegs b (Assertion p l, Nothing) segs post
structSegs b (pre, bnd) (SStmts ss : SAsrt (A.LoopInvariant p bd l) : segs) post = do
  structSStmts b (pre, bnd) ss (LoopInvariant p bd l)
  structSegs b (LoopInvariant p bd l, Just bd) segs post
structSegs b (pre, bnd) (SStmts ss : SSpec (A.Spec _ range) : segs) post = do
  post' <- wpSegs b segs post
  pre'  <- spStmts b (pre, bnd) ss
  when b (tellSpec pre' post' range)
structSegs b (pre, _) (SSpec (A.Spec _ range) : segs) post = do
  post' <- wpSegs b segs post
  when b (tellSpec pre post' range)
structSegs _ _ _ _ = error "Missing case in structSegs"

 -- 'simple' version of struct stmts -- there are no assertions,
 -- invariants, or specs in the list of statements.
structSStmts :: Bool -> (Pred, Maybe A.Expr) -> [A.Stmt] -> Pred -> WP ()
structSStmts _ (pre, _) [] post = do
  case locOf pre of
    NoLoc -> tellPO pre post (AtAssertion (locOf post))
    others -> tellPO pre post (AtAssertion others)
  return ()
structSStmts b (pre, bnd) (stmt : stmts) post = do
  post' <- wpSStmts b stmts post
  struct b (pre, bnd) stmt post'

{-
structStmts :: Bool -> Pred -> Maybe A.Expr -> [A.Stmt] -> Pred -> WP ()
structStmts _ pre _ [] post = do
  case locOf pre of
    NoLoc -> tellPO pre post (AtAssertion (locOf post))
    others -> tellPO pre post (AtAssertion others)
  return ()
structStmts b pre _ (A.Assert p l : stmts) post = do
  pre' <-
    if b
      then do
        let assert = Assertion p l
        tellPO pre assert (AtAssertion l)
        return assert
      else return pre
  structStmts b pre' Nothing stmts post
structStmts True pre _ (A.LoopInvariant p bnd l : stmts) post = do
  let origin = if startsWithDo stmts then AtLoop l else AtAssertion l
  let loopInv = LoopInvariant p bnd l
  tellPO pre loopInv origin
  structStmts True loopInv (Just bnd) stmts post
  where
    startsWithDo :: [A.Stmt] -> Bool
    startsWithDo (A.Do _ _ : _) = True
    startsWithDo _ = False
structStmts False pre _ (A.LoopInvariant {} : stmts) post = do
  structStmts False pre Nothing stmts post
structStmts b pre bnd (stmt : stmts) post = do
  post' <- wpStmts b stmts post
  struct b pre bnd stmt post'
-}

struct :: Bool -> (Pred, Maybe A.Expr) -> A.Stmt -> Pred -> WP ()
struct _ (pre, _) (A.Abort l) _ = tellPO pre (Constant A.false) (AtAbort l)
struct _ (pre, _) (A.Skip l) post = tellPO pre post (AtSkip l)
struct _ (pre, _) (A.Assign xs es l) post = do
  let sub = Map.fromList . zip xs . map Left $ es :: Subs Bindings
  tellPO pre (subst sub post) (AtAssignment l)
-- SCM: assertions and specs are handled by structSegs.
-- struct True (pre, _) (A.Assert p l) post = do
--   tellPO pre (Assertion p l) (AtAssertion l)
--   tellPO (Assertion p l) post (AtAssertion l)
-- struct False (pre, _) (A.Assert _ l) post = do
--   tellPO pre post (AtAssertion l)
-- struct _ (pre, _) (A.LoopInvariant p b l) post = do
--   tellPO pre (LoopInvariant p b l) (AtAssertion l)
--   tellPO (LoopInvariant p b l) post (AtAssertion l)
struct b (pre, _) (A.If gcmds l) post = do
  when b $ tellPO pre (disjunctGuards gcmds) (AtIf l)
  forM_ gcmds $ \(A.GdCmd guard body _) ->
    structStmts b (Conjunct [pre, guardIf guard], Nothing) body post
struct True (inv, Just bnd) (A.Do gcmds l) post = do
  let guards = A.getGuards gcmds
  tellPO (Conjunct (inv : map (Negate . guardLoop) guards)) post (AtLoop l)
  forM_ gcmds (structGdcmdInduct inv)
  tellPO
    (Conjunct (inv : map guardLoop guards))
    (Bound (bnd `A.gte` A.Lit (A.Num 0) NoLoc) NoLoc)
    (AtTermination l)
  forM_ gcmds (structGdcmdBnd inv bnd)
struct False (inv, _) (A.Do gcmds l) post = do
  let guards = A.getGuards gcmds
  tellPO (Conjunct (inv : map (Negate . guardLoop) guards)) post (AtLoop l)
struct _ (inv, Nothing) (A.Do gcmds l) post = do
  case fromLoc l of
    Nothing -> return ()
    Just rng -> throwWarning (MissingBound rng)
  let guards = A.getGuards gcmds
  tellPO (Conjunct (inv : map (Negate . guardLoop) guards)) post (AtLoop l)
  forM_ gcmds (structGdcmdInduct inv)
--  tellPO (Conjunct (inv : map guardLoop guards)) post (AtTermination l)
-- struct b (pre, _) (A.Spec _ range) post = when b (tellSpec pre post range)
struct _ _ (A.Proof _) _ = return ()
struct _ _ _ _ = error "missing case in struct"

structGdcmdInduct :: Pred -> A.GdCmd -> WP ()
structGdcmdInduct inv (A.GdCmd guard body _) =
  structStmts True (Conjunct [inv, guardLoop guard], Nothing) body inv

structGdcmdBnd :: Pred -> A.Expr -> A.GdCmd -> WP ()
structGdcmdBnd inv bnd (A.GdCmd guard body _) = do
  oldbnd <- freshText
  structStmts
    False
    ( Conjunct
        [ inv,
          Bound (bnd `A.eqq` A.Var (Name oldbnd NoLoc) NoLoc) NoLoc,
          guardLoop guard
        ],
    Nothing )
    body
    (Bound (bnd `A.lt` A.Var (Name oldbnd NoLoc) NoLoc) NoLoc)

-- weakest precondition

wpStmts :: Bool -> [A.Stmt] -> Pred -> WP Pred
wpStmts b stmts post =
  wpSegs b (groupStmts stmts) post

wpSegs :: Bool -> [SegElm] -> Pred -> WP Pred
wpSegs _ [] post = return post
wpSegs b (SStmts ss : segs) post = do
  post' <- wpSegs b segs post
  wpSStmts b ss post'
wpSegs b (SSpec (A.Spec _ range) : segs) post = do
  post' <- wpSegs b segs post
  when b (tellSpec post' post' range)
  return post'
wpSegs b (SAsrt (A.Assert p l) : segs) post = do
  structSegs b (Assertion p l, Nothing) segs post
  return (Assertion p l)
wpSegs b (SAsrt (A.LoopInvariant p bd l) : segs) post = do
  structSegs b (LoopInvariant p bd l, Just bd) segs post
  return (Assertion p l) -- SCM: erasing bound information?
wpSegs _ _ _ = error "Missing case in wpSegs"

  -- wpStmts need not deal with assertions and specs

wpSStmts :: Bool -> [A.Stmt] -> Pred -> WP Pred
wpSStmts _ [] post = return post
wpSStmts b (stmt : stmts) post = do
  post' <- wpSStmts b stmts post
  wp b stmt post'
-- wpStmts True (A.Assert pre l : stmts) post = do
--   structStmts True (Assertion pre l, Nothing) stmts post
--   return (Assertion pre l)
-- wpStmts False (A.Assert {} : stmts) post =
--   wpStmts False stmts post
-- wpStmts True (A.LoopInvariant p b l : stmts) post = do
--   structStmts True (LoopInvariant p b l, Just b) stmts post
--   return (LoopInvariant p b l)
-- wpStmts False (A.LoopInvariant {} : stmts) post = do
--   wpStmts False stmts post

wp :: Bool -> A.Stmt -> Pred -> WP Pred
wp _ (A.Skip _) post = return post
wp _ (A.Abort _) _ = return (Constant A.false)
wp _ (A.Assign xs es _) post = do
  return $ subst sub post
  where
    sub :: Subs Bindings
    sub = Map.fromList . zip xs . map Left $ es
wp _ (A.Assert p l) post = error "wp got assert"
-- wp _ (A.Assert p l) post = do
--   tellPO (Assertion p l) post (AtAssertion l)
--   return (Assertion p l)
wp _ (A.LoopInvariant p b l) post = error "wp got inv"
-- wp _ (A.LoopInvariant p b l) post = do
--   tellPO (LoopInvariant p b l) post (AtAssertion l)
--   return (LoopInvariant p b l)
wp _ (A.Do _ l) _ = throwError $ MissingAssertion l
wp b (A.If gcmds _) post = do
  pres <- forM gcmds $ \(A.GdCmd guard body _) ->
    Constant . (guard `A.imply`)
      . toExpr
      <$> wpStmts b body post
  return (conjunct (disjunctGuards gcmds : pres))
wp b (A.Spec _ range) post = error "wp got spec"
-- wp b (A.Spec _ range) post = do
--   when b (tellSpec post post range)
--   return post
wp _ (A.Proof _) post = return post
wp _ _ _ = error "missing case in wp"

disjunctGuards :: [A.GdCmd] -> Pred
disjunctGuards = disjunct . map guardIf . A.getGuards

-- strongest postcondition

spStmts :: Bool -> (Pred, Maybe A.Expr) -> [A.Stmt] -> WP Pred
spStmts _ (pre, _) _ = return pre
-- spStmts _ (pre, _) [] = return pre
-- spStmts b (stmt : stmts) post = do
--

--

tellPO :: Pred -> Pred -> Origin -> WP ()
tellPO p q l = unless (toExpr p == toExpr q) $ do
  (i, j, k) <- get
  put (succ i, j, k)
  tell ([PO i p q l], [], [])

tellSpec :: Pred -> Pred -> Range -> WP ()
tellSpec p q l = do
  (i, j, k) <- get
  put (i, succ j, k)
  tell ([], [Specification j p q l], [])

throwWarning :: StructWarning -> WP ()
throwWarning warning = do
  tell ([], [], [warning])

data StructWarning
  = MissingBound Range
  | ExcessBound Range
  deriving (Eq, Show, Generic)

instance Located StructWarning where
  locOf (MissingBound rng) = locOf rng
  locOf (ExcessBound rng) = locOf rng

data StructError
  = MissingAssertion Loc
  | MissingPostcondition Loc
  deriving (Eq, Show, Generic)

instance Located StructError where
  locOf (MissingAssertion l) = l
  locOf (MissingPostcondition l) = l

instance ToJSON StructError
