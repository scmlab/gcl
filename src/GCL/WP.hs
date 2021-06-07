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
import GCL.Common (Bindings, Fresh (fresh, freshText), Subs, Substitutable (apply))
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

-- applyPredAssert :: Env (Either A.Expr A.Expr) -> A.Expr -> Loc -> Pred
-- applyPredAssert env p l = apply env (Assertion p l)

-- applyPredAssert' :: A.Expr -> Loc -> WP Pred
-- applyPredAssert' p l = do
--   env <- ask
--   let env' = Map.map Right env :: Env (Either A.Expr A.Expr)
--   return $ apply env' (Assertion p l)

-- applyPredLoopInvariant :: A.Expr -> A.Expr -> Loc -> WP Pred
-- applyPredLoopInvariant p bnd l = do
--   env <- ask
--   let env' = Map.map Right env :: Env (Either A.Expr A.Expr)
--   return $ apply env' (LoopInvariant p bnd l)

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

  case progView (apply env stmts) of
    ProgViewEmpty -> return ()
    ProgViewOkay pre stmts' post -> structStmts True pre Nothing stmts' post
    ProgViewMissingPrecondition stmts' post -> structStmts True (Constant A.true) Nothing stmts' post
    ProgViewMissingPostcondition _ stmts' -> throwError . MissingPostcondition . locOf . last $ stmts'
    ProgViewMissingBoth stmts' -> throwError . MissingPostcondition . locOf . last $ stmts'

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

struct :: Bool -> Pred -> Maybe A.Expr -> A.Stmt -> Pred -> WP ()
struct _ pre _ (A.Abort l) _ = tellPO pre (Constant A.false) (AtAbort l)
struct _ pre _ (A.Skip l) post = tellPO pre post (AtSkip l)
struct _ pre _ (A.Assign xs es l) post = do
  let subst = Map.fromList . zip xs . map Left $ es :: Subs Bindings
  tellPO pre (apply subst post) (AtAssignment l)
struct True pre _ (A.Assert p l) post = do
  tellPO pre (Assertion p l) (AtAssertion l)
  tellPO (Assertion p l) post (AtAssertion l)
struct False pre _ (A.Assert _ l) post = do
  tellPO pre post (AtAssertion l)
struct _ pre _ (A.LoopInvariant p b l) post = do
  tellPO pre (LoopInvariant p b l) (AtAssertion l)
  tellPO (LoopInvariant p b l) post (AtAssertion l)
struct b pre _ (A.If gcmds l) post = do
  when b $ tellPO pre (disjunctGuards gcmds) (AtIf l)
  forM_ gcmds $ \(A.GdCmd guard body _) ->
    structStmts b (Conjunct [pre, guardIf guard]) Nothing body post
struct True inv (Just bnd) (A.Do gcmds l) post = do
  let guards = A.getGuards gcmds
  tellPO (Conjunct (inv : map (Negate . guardLoop) guards)) post (AtLoop l)
  forM_ gcmds (structGdcmdInduct inv)
  tellPO
    (Conjunct (inv : map guardLoop guards))
    (Bound (bnd `A.gte` A.Lit (A.Num 0) NoLoc) NoLoc)
    (AtTermination l)
  forM_ gcmds (structGdcmdBnd inv bnd)
struct False inv _ (A.Do gcmds l) post = do
  let guards = A.getGuards gcmds
  tellPO (Conjunct (inv : map (Negate . guardLoop) guards)) post (AtLoop l)
struct _ inv Nothing (A.Do gcmds l) post = do
  case fromLoc l of
    Nothing -> return ()
    Just rng -> throwWarning (MissingBound rng)
  let guards = A.getGuards gcmds
  tellPO (Conjunct (inv : map (Negate . guardLoop) guards)) post (AtLoop l)
  forM_ gcmds (structGdcmdInduct inv)
  tellPO (Conjunct (inv : map guardLoop guards)) post (AtTermination l)
struct b pre _ (A.Spec _ range) post = when b (tellSpec pre post range)
struct _ _ _ (A.Proof _) _ = return ()

structGdcmdInduct :: Pred -> A.GdCmd -> WP ()
structGdcmdInduct inv (A.GdCmd guard body _) =
  structStmts True (Conjunct [inv, guardLoop guard]) Nothing body inv

structGdcmdBnd :: Pred -> A.Expr -> A.GdCmd -> WP ()
structGdcmdBnd inv bnd (A.GdCmd guard body _) = do
  oldbnd <- freshText
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

wpStmts :: Bool -> [A.Stmt] -> Pred -> WP Pred
wpStmts _ [] post = return post
wpStmts True (A.Assert pre l : stmts) post = do
  structStmts True (Assertion pre l) Nothing stmts post
  return (Assertion pre l)
wpStmts False (A.Assert {} : stmts) post =
  wpStmts False stmts post
wpStmts True (A.LoopInvariant p b l : stmts) post = do
  structStmts True (LoopInvariant p b l) (Just b) stmts post
  return (LoopInvariant p b l)
wpStmts False (A.LoopInvariant {} : stmts) post = do
  wpStmts False stmts post
wpStmts b (stmt : stmts) post = do
  post' <- wpStmts b stmts post
  wp b stmt post'

wp :: Bool -> A.Stmt -> Pred -> WP Pred
wp _ (A.Skip _) post = return post
wp _ (A.Abort _) _ = return (Constant A.false)
wp _ (A.Assign xs es _) post = do
  return $ apply subst post
  where
    subst :: Subs Bindings
    subst = Map.fromList . zip xs . map Left $ es
wp _ (A.Assert p l) post = do
  tellPO (Assertion p l) post (AtAssertion l)
  return (Assertion p l)
wp _ (A.LoopInvariant p b l) post = do
  tellPO (LoopInvariant p b l) post (AtAssertion l)
  return (LoopInvariant p b l)
wp _ (A.Do _ l) _ = throwError $ MissingAssertion l
wp b (A.If gcmds _) post = do
  pres <- forM gcmds $ \(A.GdCmd guard body _) ->
    Constant . (guard `A.imply`)
      . toExpr
      <$> wpStmts b body post
  return (conjunct (disjunctGuards gcmds : pres))
wp b (A.Spec _ range) post = do
  when b (tellSpec post post range)
  return post
wp _ (A.Proof _) post = return post

disjunctGuards :: [A.GdCmd] -> Pred
disjunctGuards = disjunct . map guardIf . A.getGuards

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