{-# LANGUAGE OverloadedStrings, FlexibleContexts, DeriveGeneric,
             TypeSynonymInstances, FlexibleInstances #-}

module GCL.WP where

import Control.Monad.State hiding (guard)
import Control.Monad.Writer hiding (guard)
import Control.Monad.Except hiding (guard)

import qualified Data.Map as Map
import Data.Loc (Loc(..), Located(..))
import Data.Aeson
import GHC.Generics

import Syntax.Concrete (Expr, Stmt, Lower)
import qualified Syntax.Concrete as C
-- import qualified Syntax.Predicate as P
import Syntax.Predicate
import Syntax.Location (ToNoLoc(..))

data Obligation
  = Obligation Int Pred Pred ObliOrigin
  deriving (Eq, Show, Generic)

instance ToNoLoc Obligation where
  toNoLoc (Obligation i p q o) = Obligation i (toNoLoc p) (toNoLoc q) (toNoLoc o)

data Specification = Specification
  { specID       :: Int
  , specPreCond  :: Pred
  , specPostCond :: Pred
  , specLoc      :: Loc
  } deriving (Eq, Show, Generic)

instance ToNoLoc Specification where
  toNoLoc (Specification i p q _) = Specification i (toNoLoc p) (toNoLoc q) NoLoc

data ObliOrigin = AroundAbort Loc
                | AroundSkip Loc
                | AssertGuaranteed Loc
                | AssertSufficient Loc
                | Assignment Loc
                | IfTotal Loc
                | LoopBase Loc
                | LoopTermBase Loc
                | LoopInitialize Loc
      deriving (Eq, Show, Generic)

instance ToNoLoc ObliOrigin where
  toNoLoc (AroundAbort _)       = AroundAbort NoLoc
  toNoLoc (AroundSkip _)        = AroundSkip NoLoc
  toNoLoc (AssertGuaranteed _)  = AssertGuaranteed NoLoc
  toNoLoc (AssertSufficient _)  = AssertSufficient NoLoc
  toNoLoc (Assignment _)        = Assignment NoLoc
  toNoLoc (IfTotal _)           = IfTotal NoLoc
  toNoLoc (LoopBase _)          = LoopBase NoLoc
  toNoLoc (LoopTermBase _)      = LoopTermBase NoLoc
  toNoLoc (LoopInitialize _)    = LoopInitialize NoLoc

type SM = WriterT [Obligation] (WriterT [Specification]
                (StateT (Int, Int, Int)
                  (Either StructError)))

-- create a proof obligation
tellObli :: Obligation -> SM ()
tellObli obligation = tell [obligation]

obligate :: Pred -> Pred -> ObliOrigin -> SM ()
obligate p q l = do
  -- NOTE: this could use some love
  unless (C.predEq (toExpr p) (toExpr q)) $ do
    (i, j, k) <- get
    put (succ i, j, k)
    tellObli $ Obligation i p q l

-- inform existence of a spec hole

tellSpec :: Pred -> Pred -> Loc -> SM ()
tellSpec p q loc = do
  (i, j, k) <- get
  put (i, succ j, k)
  lift $ tell [Specification j p q loc]

--------------------------------------------------------------------------------
-- | Structure, and Weakest-Precondition

struct :: Bool -> Pred -> Maybe Expr -> Stmt -> Pred -> SM ()

struct _ pre _ (C.Abort l) _ = obligate pre (Constant C.false) (AroundAbort l)

struct _ pre _ (C.Skip l) post = obligate pre post (AroundSkip l)

struct _ pre _ (C.Assert p l) post = do
   obligate pre (Assertion p l) (AssertGuaranteed l)
   obligate (Assertion p l) post (AssertSufficient l)

struct _ _ _ (C.LoopInvariant _ _ l) _ =
   throwError (ExcessBound l)

struct _ pre _ (C.Assign xs es l) post = do
  post' <- subst (assignmentEnv xs es) post
  obligate pre post' (Assignment l)

struct b pre _ (C.If gcmds l) post = do
  obligate pre (Disjunct $ map (toGuard (IF l)) $ C.getGuards gcmds) (IfTotal l)
  forM_ gcmds $ \(C.GdCmd guard body _) ->
    structStmts b (Conjunct [pre, toGuard (IF l) guard]) Nothing body post

struct _ _ Nothing (C.Do _ l) _ =
  throwError (MissingBound l)
 {- Or if we want to tolerate the user and carry on ---
 do -- warn that bnd is missing
  let gcmds' = map (\(GdCmd x y _) -> (depart x, y)) gcmds
  let guards = map fst gcmds'
  obligate (inv `A.conj` (A.conjunct (map A.neg guards))) post
  --
  forM_ gcmds' $ \(guard, body) -> do
    structStmts b (inv `A.conj` guard) Nothing body inv
 -}

struct b inv (Just bnd) (C.Do gcmds l) post = do
  -- base case
  let guards = C.getGuards gcmds
  obligate (Conjunct (inv : (map (Negate . toGuard (LOOP l)) guards))) post (LoopBase l)
  -- inductive cases
  forM_ gcmds $ \(C.GdCmd guard body _) ->
    structStmts b (Conjunct [inv, (toGuard (LOOP l)) guard]) Nothing body inv
  -- termination
  obligate (Conjunct (inv : map (toGuard (LOOP l)) guards))
       (Bound (bnd `C.gte` (C.Lit (C.Num 0) NoLoc)) NoLoc) (LoopTermBase l)
  -- bound decrementation
  oldbnd <- C.freshVar "bnd"
  forM_ gcmds $ \(C.GdCmd guard body _) ->
    structStmts
      False
      (Conjunct
        [ inv
        , Bound (bnd `C.eqq` C.Var oldbnd NoLoc) NoLoc
        , toGuard (LOOP l) guard
        ])
      Nothing
      body
      (Bound (bnd `C.lt` C.Var oldbnd NoLoc) NoLoc)

struct _ _ _ (C.SpecQM l) _    = throwError $ DigHole l
struct b pre _ (C.Spec l) post = when b (tellSpec pre post l)


structStmts :: Bool -> Pred -> Maybe Expr -> [Stmt] -> Pred -> SM ()

structStmts _ _ _ [] _ = return ()

structStmts b pre _ (C.Assert p l : stmts) post = do
  obligate pre (Assertion p l) (AssertGuaranteed l)
  structStmts b (Assertion p l) Nothing stmts post

structStmts b pre _ (C.LoopInvariant p bnd l : stmts) post = do
  let origin = if startsWithDo stmts
                    then LoopInitialize l
                    else AssertGuaranteed l
  obligate pre (LoopInvariant p bnd l) origin
  structStmts b (LoopInvariant p bnd l) (Just bnd) stmts post

 where startsWithDo (C.Do _ _ : _) = True
       startsWithDo _ = False

structStmts b pre bnd (stmt : stmts) post = do
  post' <- wpStmts b stmts post
  struct b pre bnd stmt post'

structProg :: [Stmt] -> SM ()
structProg [] = return ()
structProg (C.Assert pre l1 : stmts) =
  case (init stmts, last stmts) of
    (stmts', C.Assert post l2) ->
       structStmts True (Assertion pre l1) Nothing stmts' (Assertion post l2)
    (_, stmt) -> throwError (MissingPostcondition (locOf stmt))
structProg stmts =
  case (init stmts, last stmts) of
    (stmts', C.Assert post l) ->
       structStmts True (Constant C.true) Nothing stmts' (Assertion post l)
    (_, stmt) -> throwError (MissingPostcondition (locOf stmt))


wpStmts :: Bool -> [Stmt] -> Pred -> SM Pred

wpStmts _ [] post = return post

wpStmts b (C.Assert pre l : stmts) post =
  structStmts b (Assertion pre l) Nothing stmts post >>
  return (Assertion pre l)

wpStmts b (C.LoopInvariant pre bnd l : stmts) post =
  structStmts b (LoopInvariant pre bnd l) (Just bnd) stmts post >>
  return (LoopInvariant pre bnd l)

wpStmts b (stmt : stmts) post = do
  post' <- wpStmts b stmts post
  wp b stmt post'

wp :: Bool -> Stmt -> Pred -> SM Pred

wp _ (C.Abort _) _ = return (Constant C.false)

wp _ (C.Skip _) post = return post

wp _ (C.Assert p l) post = do
  obligate (Assertion p l) post (AssertSufficient l)
  return (Assertion p l)

wp _ (C.LoopInvariant p b l) post = do
  obligate (LoopInvariant p b l) post (AssertSufficient l)
  return (LoopInvariant p b l)

wp _ (C.Assign xs es _) post =
  subst (assignmentEnv xs es) post

wp b (C.If gcmds l) post = do
  forM_ gcmds $ \(C.GdCmd guard body _) ->
    structStmts b (toGuard (IF l) guard) Nothing body post
  return (Disjunct (map (toGuard (IF l)) $ C.getGuards gcmds)) -- is this enough?

wp _ (C.Do _ l) _ = throwError (MissingAssertion l)

wp _ (C.SpecQM l) _ = throwError $ DigHole l

wp b (C.Spec l) post = do
  when b (tellSpec post post l)
  return post  -- not quite right

wpProg :: [Stmt] -> SM Pred
wpProg [] = return (Constant C.true)
wpProg stmts =
  case (init stmts, last stmts) of
    (stmts', C.Assert p l) -> wpStmts True stmts' (Assertion p l)
    (_, stmt) -> throwError (MissingPostcondition (locOf stmt))

assignmentEnv :: [Lower] -> [Expr] -> C.Subst
assignmentEnv xs es = Map.fromList (zip (map Left xs) es)

--------------------------------------------------------------------------------
-- | The monad, and other supportive operations

instance C.Fresh SM where
  fresh = do (i, j, k) <- get
             put (i, j, succ k)
             return k

runSM :: SM a -> (Int, Int, Int) -> Either StructError
            (((a, [Obligation]), [Specification]), (Int, Int, Int))
runSM p s = runStateT (runWriterT . runWriterT $ p) s

runWP :: SM a -> Either StructError ((a, [Obligation]), [Specification])
runWP p = fmap fst $ runSM p (0,0,0)

instance Located ObliOrigin where
  locOf (AroundAbort      l) = l
  locOf (AroundSkip       l) = l
  locOf (AssertGuaranteed l) = l
  locOf (AssertSufficient l) = l
  locOf (Assignment       l) = l
  locOf (IfTotal          l) = l
  locOf (LoopBase         l) = l
  locOf (LoopTermBase     l) = l
  locOf (LoopInitialize   l) = l

-- instance Located Obligation where
--   locOf (Obligation _ _ _ o) = locOf o

censorSpec :: ([Specification] -> [Specification]) -> SM a -> SM a
censorSpec f = mapWriterT (censor f)


data StructError = MissingAssertion Loc
                 | MissingBound Loc
                 | ExcessBound  Loc
                 | MissingPostcondition Loc
                 | DigHole Loc
                deriving (Eq, Show, Generic)

instance Located StructError where
  locOf (MissingAssertion loc) = loc
  locOf (MissingBound     loc) = loc
  locOf (ExcessBound      loc) = loc
  locOf (MissingPostcondition loc) = loc
  locOf (DigHole loc) = loc

instance ToJSON StructError where
