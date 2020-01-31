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

import Syntax.Concrete
import qualified Syntax.Abstract as A
import Syntax.Location ()

type Index = A.Index



data Obligation
  = Obligation Index Pred Pred [ObliOrigin]
  deriving (Show, Generic)

data Specification = Specification
  { specID       :: Int
  , specPreCond  :: Pred
  , specPostCond :: Pred
  , specLoc      :: Loc
  } deriving (Show, Generic)

data ObliOrigin = AroundAbort Loc
                | AroundSkip Loc
                | AssertGuaranteed Loc
                | AssertSufficient Loc
                | Assignment Loc
                | IfTotal Loc
                | IfBranch Loc
                | LoopBase Loc
                | LoopInd Loc
                | LoopTermBase Loc
                | LoopTermDec Loc
                | LoopInitialize Loc
      deriving (Show, Generic)

type SM = WriterT [Obligation] (WriterT [Specification]
                (StateT (Int, Int, Int)
                  (Either StructError)))

-- create a proof obligation
tellObli :: Obligation -> SM ()
tellObli obligation = tell [obligation]

obligate :: Pred -> Pred -> ObliOrigin -> SM ()
obligate p q l = do
  -- NOTE: this could use some love
  unless (predEq (predToExpr p) (predToExpr q)) $ do
    (i, j, k) <- get
    put (succ i, j, k)
    tellObli $ Obligation i p q [l]

-- inform existence of a spec hole

tellSpec :: Pred -> Pred -> Loc -> SM ()
tellSpec p q loc = do
  (i, j, k) <- get
  put (i, succ j, k)
  lift $ tell [Specification j p q loc]

--------------------------------------------------------------------------------
-- | Structure, and Weakest-Precondition

struct :: Bool -> Pred -> Maybe Expr -> Stmt -> Pred -> SM ()

struct _ pre _ (Abort l) _ = obligate pre (Pred false) (AroundAbort l)

struct _ pre _ (Skip l) post = obligate pre post (AroundSkip l)

struct _ pre _ (Assert p l) post = do
   obligate pre (Assertion p) (AssertGuaranteed l)
   obligate (Assertion p) post (AssertSufficient l)

struct _ _ _ (AssertWithBnd _ _ l) _ =
   throwError (ExcessBound l)

struct _ pre _ (Assign xs es l) post = do
  post' <- substPred (assignmentEnv xs es) post
  obligate pre post' (Assignment l)

struct b pre _ (If gcmds l) post = do
  obligate pre (Disjunct $ map Guard $ getGuards gcmds) (IfTotal l)
  forM_ gcmds $ \(GdCmd guard body l') ->
    addObliOrigin (IfBranch l')
     (structStmts b (Conjunct [pre, Guard guard]) Nothing body post)

struct _ _ Nothing (Do _ l) _ =
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

struct b inv (Just bnd) (Do gcmds l) post = do
  -- base case
  let guards = getGuards gcmds
  obligate (Conjunct (inv : (map (Guard . neg) guards))) post (LoopBase l)
  -- obligate (LoopBaseConj inv (conjunct (map neg guards))) post (LoopBase l)
  -- inductive cases
  forM_ gcmds $ \(GdCmd guard body l') ->
    addObliOrigin (LoopInd l')
      (structStmts b (Conjunct [inv, Guard guard]) Nothing body inv)
  -- termination
  obligate (Conjunct (inv : map Guard guards))
       (Pred $ bnd `gte` (Lit (Num 0) NoLoc)) (LoopTermBase l)
  -- bound decrementation
  oldbnd <- freshVar "bnd"
  forM_ gcmds $ \(GdCmd guard body l') ->
    addObliOrigin (LoopTermDec l')
      (structStmts
            False
            (Conjunct
              [ inv
              , Pred $ bnd `eqq` Var oldbnd NoLoc
              , Guard guard
              ])
            Nothing
            body
            (Pred $ bnd `lte` Var oldbnd NoLoc))

struct _ _ _ (SpecQM l) _    = throwError $ DigHole l
struct b pre _ (Spec l) post = when b (tellSpec pre post l)


structStmts :: Bool -> Pred -> Maybe Expr -> [Stmt] -> Pred -> SM ()

structStmts _ _ _ [] _ = return ()

structStmts b pre _ (Assert p l : stmts) post = do
  obligate pre (Assertion p) (AssertGuaranteed l)
  structStmts b (Assertion p) Nothing stmts post

structStmts b pre _ (AssertWithBnd p bnd l : stmts) post = do
  let origin = if startsWithDo stmts
                    then LoopInitialize l
                    else AssertGuaranteed l
  obligate pre (Assertion p) origin
  structStmts b (Assertion p) (Just bnd) stmts post

 where startsWithDo (Do _ _ : _) = True
       startsWithDo _ = False

structStmts b pre bnd (stmt : stmts) post = do
  post' <- wpStmts b stmts post
  struct b pre bnd stmt post'

structProg :: [Stmt] -> SM ()
structProg [] = return ()
structProg (Assert pre _ : stmts) =
  case (init stmts, last stmts) of
    (stmts', Assert post _) ->
       structStmts True (Assertion pre) Nothing stmts' (Assertion post)
    (_, stmt) -> throwError (MissingPostcondition (locOf stmt))
structProg stmts =
  case (init stmts, last stmts) of
    (stmts', Assert post _) ->
       structStmts True (Pred true) Nothing stmts' (Assertion post)
    (_, stmt) -> throwError (MissingPostcondition (locOf stmt))


wpStmts :: Bool -> [Stmt] -> Pred -> SM Pred

wpStmts _ [] post = return post

wpStmts b (Assert pre _ : stmts) post =
  structStmts b (Assertion pre) Nothing stmts post >>
  return (Assertion pre)

wpStmts b (AssertWithBnd pre bnd _ : stmts) post =
  structStmts b (Assertion pre) (Just bnd) stmts post >>
  return (Assertion pre)

wpStmts b (stmt : stmts) post = do
  post' <- wpStmts b stmts post
  wp b stmt post'

wp :: Bool -> Stmt -> Pred -> SM Pred

wp _ (Abort _) _ = return (Pred false)

wp _ (Skip _) post = return post

wp _ (Assert p l) post = do
  obligate (Assertion p) post (AssertSufficient l)
  return (Assertion p)

wp _ (AssertWithBnd p _ l) post = do
  obligate (Assertion p) post (AssertSufficient l)
  return (Assertion p)

wp _ (Assign xs es _) post =
  substPred (assignmentEnv xs es) post

wp b (If gcmds _) post = do
  forM_ gcmds $ \(GdCmd guard body _) ->
    structStmts b (Guard guard) Nothing body post
  return (Disjunct (map Guard $ getGuards gcmds)) -- is this enough?

wp _ (Do _ l) _ = throwError (MissingAssertion l)

wp _ (SpecQM l) _ = throwError $ DigHole l

wp b (Spec l) post = do
  when b (tellSpec post post l)
  return post  -- not quite right

wpProg :: [Stmt] -> SM Pred
wpProg [] = return (Pred true)
wpProg stmts =
  case (init stmts, last stmts) of
    (stmts', Assert p _) -> wpStmts True stmts' (Assertion p)
    (_, stmt) -> throwError (MissingPostcondition (locOf stmt))

assignmentEnv :: [Lower] -> [Expr] -> Subst
assignmentEnv xs es = Map.fromList (zip (map Left xs) es)

--------------------------------------------------------------------------------
-- | The monad, and other supportive operations

instance Fresh SM where
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
  locOf (IfBranch         l) = l
  locOf (LoopBase         l) = l
  locOf (LoopInd          l) = l
  locOf (LoopTermBase     l) = l
  locOf (LoopTermDec      l) = l
  locOf (LoopInitialize   l) = l

-- instance Located Obligation where
--   locOf (Obligation _ _ _ o) = locOf o

censorObli :: ([Obligation] -> [Obligation]) -> SM a -> SM a
censorObli = censor

addObliOrigin :: ObliOrigin -> SM a -> SM a
addObliOrigin ori =
  censorObli (map (\(Obligation i p q os) -> Obligation i p q (ori:os)))

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


--------------------------------------------------------------------------------
-- | Predicates

data Pred = Pred              Expr
          | Assertion         Expr
          | Guard             Expr
          | Conjunct  [Pred]
          | Disjunct  [Pred]
          -- | Imply      Pred  Pred
          -- | Negate     Pred
          deriving (Show, Generic)

instance ToJSON Pred where

predToExpr :: Pred -> Expr
predToExpr (Pred e) = e
predToExpr (Assertion e) = e
predToExpr (Guard e) = e
predToExpr (Conjunct xs) = conjunct (map predToExpr xs)
predToExpr (Disjunct xs) = disjunct (map predToExpr xs)

-- predToExpr (Imply p q) = imply (predToExpr p) (predToExpr q)
-- predToExpr (Negate p) = neg (predToExpr p)

substPred :: Subst -> Pred -> SM Pred
substPred env (Pred e) = Pred <$> subst env e
substPred env (Assertion e) = Assertion <$> subst env e
substPred env (Guard e) = Guard <$> subst env e
substPred env (Conjunct xs) = Conjunct <$> mapM (substPred env) xs
substPred env (Disjunct es) = Disjunct <$> mapM (substPred env) es
