{-# LANGUAGE OverloadedStrings, FlexibleContexts, DeriveGeneric,
             TypeSynonymInstances, FlexibleInstances #-}

module GCL.WP where

import Control.Monad.State hiding (guard)
import Control.Monad.Writer hiding (guard)
import Control.Monad.Except hiding (guard)

import Data.Loc (Loc(..), Located(..))
import Data.Aeson
import GHC.Generics

import Syntax.Concrete
import Syntax.Abstract (Fresh(..))
import qualified Syntax.Abstract as A
import Syntax.Abstract.Location

type Pred = A.Expr
type Index = A.Index

data Obligation = Obligation Index Pred Pred deriving (Show, Generic)
data Hardness = Hard | Soft deriving (Show, Generic) -- should be depreciated
data Specification = Specification
  { specID       :: Int
  , specHardness :: Hardness
  , specPreCond  :: Pred
  , specPostCond :: Pred
  , specLoc      :: Loc
  } deriving (Show, Generic)

type SM = WriterT [Obligation] (WriterT [Specification]
                (StateT (Int, Int, Int)
                  (Either StructError)))

data StructError = MissingAssertion Loc
                 | MissingBound Loc
                 | ExcessBound  Loc
                 | MissingPostcondition Loc
      deriving (Eq, Show, Generic)

-- create a proof obligation

obligate :: Pred -> Pred -> SM ()
obligate p q = do
  -- NOTE: this could use some love
  unless (A.predEq p q) $ do
    (i, j, k) <- get
    put (succ i, j, k)
    tell [Obligation i p q]

-- inform existence of a spec hole

tellSpec :: Hardness -> Pred -> Pred -> Loc -> SM ()
tellSpec h p q loc = do
  (i, j, k) <- get
  put (i, succ j, k)
  lift $ tell [Specification j h p q loc]

--------------------------------------------------------------------------------
-- | Structure, and Weakest-Precondition

struct :: Bool -> Pred -> Maybe (A.Expr) -> Stmt -> Pred -> SM ()

struct _ pre _ (Abort _) _ = obligate pre A.ff

struct _ pre _ (Skip _) post = obligate pre post

struct _ pre _ (Assert p _) post =
   obligate pre p' >> obligate p' post
  where p' = depart p

struct _ _ _ (AssertWithBnd _ _ l) _ =
   throwError (ExcessBound l)

struct _ pre _ (Assign xs es _) post = do
  post' <- A.subst (zip (map lowerToText xs)
                        (map depart es)) post
  obligate pre post'

struct b pre _ (If gcmds _) post = do
  let guards = map depart (getGuards gcmds)
  obligate pre (A.disjunct guards)
  forM_ gcmds $ \(GdCmd guard body _) ->
    structStmts b (pre `A.conj` depart guard) Nothing body post

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

struct b inv (Just bnd) (Do gcmds _) post = do
  -- base case
  let gcmds' = map (\(GdCmd x y _) -> (depart x, y)) gcmds
  let guards = map fst gcmds'
  obligate (inv `A.conj` (A.conjunct (map A.neg guards))) post
  -- inductive cases
  forM_ gcmds' $ \(guard, body) -> do
    structStmts b (inv `A.conj` guard) Nothing body inv
  -- termination
  obligate (inv `A.conj` A.disjunct guards) (bnd `A.gte` (A.Lit (A.Num 0)))
  -- bound decrementation
  oldbnd <- freshVar "bnd"
  let invB = inv `A.conj` (bnd `A.eqq` A.Var oldbnd)
  forM_ gcmds' $ \(guard, body) -> do
    structStmts False (inv `A.conj` guard) Nothing body invB

struct b pre _ (SpecQM l) post = when b (tellSpec Soft pre post l)
struct b pre _ (Spec l) post = when b (tellSpec Soft pre post l)


structStmts :: Bool -> Pred -> Maybe (A.Expr) -> [Stmt] -> Pred -> SM ()

structStmts _ _ _ [] _ = return ()

structStmts b pre _ (Assert p _ : stmts) post =
  obligate pre p' >>
  structStmts b p' Nothing stmts post
 where p' = depart p

structStmts b pre _ (AssertWithBnd p bnd _ : stmts) post =
  let (p', bnd') = (depart p, depart bnd) in
  obligate pre p' >>
  structStmts b p' (Just bnd') stmts post

structStmts b pre bnd (stmt : stmts) post = do
  post' <- wpStmts b stmts post
  struct b pre bnd stmt post'


wpStmts :: Bool -> [Stmt] -> Pred -> SM Pred

wpStmts _ [] post = return post

wpStmts b (Assert pre _ : stmts) post =
  structStmts b pre' Nothing stmts post >>
  return pre'
 where pre' = depart pre

wpStmts b (AssertWithBnd pre bnd _ : stmts) post =
  structStmts b pre' (Just bnd') stmts post >>
  return pre'
 where (pre', bnd') = (depart pre, depart bnd)

wpStmts b (stmt : stmts) post = do
  post' <- wpStmts b stmts post
  wp b stmt post'

wp :: Bool -> Stmt -> Pred -> SM Pred

wp _ (Abort _) _ = return A.ff

wp _ (Skip _) post = return post

wp _ (Assert p _) post =
   obligate p' post >> return p'
 where p' = depart p

wp _ (AssertWithBnd p _ _) post =
   obligate p' post >> return p'
 where p' = depart p

wp _ (Assign xs es _) post =
  A.subst (zip (map lowerToText xs) (map depart es)) post

wp b (If gcmds _) post = do
    forM_ gcmds $ \(GdCmd guard body _) ->
      structStmts b (depart guard) Nothing body post
    let guards = map depart (getGuards gcmds)
    return (A.disjunct guards) -- is this enough?

wp _ (Do _ l) _ = throwError (MissingAssertion l)

wp b (SpecQM l) post =
  when b (tellSpec Soft post post l) >> return post  -- not quite right

wp b (Spec l) post =
  when b (tellSpec Soft post post l) >> return post  -- not quite right

wpProg :: [Stmt] -> SM Pred
wpProg [] = return A.tt
wpProg stmts =
  case (init stmts, last stmts) of
    (stmts', Assert p _) -> wpStmts True stmts' (depart p)
    (_, stmt) -> throwError (MissingPostcondition (locOf stmt))


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

instance Located StructError where
  locOf (MissingAssertion loc) = loc
  locOf (MissingBound     loc) = loc
  locOf (ExcessBound      loc) = loc
  locOf (MissingPostcondition loc) = loc

instance ToJSON StructError where

{-
-- SCM: I thought these would be useful,
--        but it turns out that I do not need them yet.
censorObli :: ([Obligation] -> [Obligation]) -> M a -> M a
censorObli = censor

censorSpec :: ([Specification] -> [Specification]) -> M a -> M a
censorSpec f = mapWriterT (censor f)
-}
