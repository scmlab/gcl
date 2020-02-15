{-# LANGUAGE OverloadedStrings, FlexibleContexts, DeriveGeneric,
             TypeSynonymInstances, FlexibleInstances #-}

module GCL.WP2 where

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
-- import Debug.Trace (traceShow)


--------------------------------------------------------------------------------
-- | Lasagna, alternating sequence of Pred & Statement

type PreviousStmt = Maybe Stmt
data Lasagna = Layer
                  PreviousStmt  -- the previous statement
                  Pred          -- precondition
                  Stmt          -- the current statement
                  [Lasagna]     -- sub-lasagne of the current statement (IF, LOOP)
                  Lasagna       -- following layers
             | Final Pred
             deriving (Show)

-- access the precondition of a Lasagna
precond :: Lasagna -> Pred
precond (Final p) = p
precond (Layer _ p _ _ _) = p


toLasagna :: [Stmt] -> Pred -> WPM Lasagna
toLasagna = wpStmts Nothing []
  where

    -- the workhorse
    wpStmts :: PreviousStmt -> [Pred] -> [Stmt] -> Pred -> WPM Lasagna
    wpStmts _        _    []     post = return $ Final post
    wpStmts previous pres (x:xs) post = do
      xs' <- wpStmts (Just x) pres xs post
      (pre, branches) <- wp previous pres x (precond xs')
      return $ Layer previous pre x branches xs'

    -- calculates the weakest precondition of a given statement
    -- along with the postcondition and its previous statement
    wp :: PreviousStmt -> [Pred] -> Stmt -> Pred -> WPM (Pred, [Lasagna])
    wp previous pres current post = case (previous, current) of
      (_, C.Abort _)              -> return (Constant C.false, [])
      (_, C.Skip _)               -> return (post, [])
      (_, C.Assert p l)           -> return (Assertion p l, [])
      (_, C.LoopInvariant p _ l)  -> return (LoopInvariant p l, [])
      (_, C.Assign xs es _)       -> do
        pre <- subst (assignmentEnv xs es) post
        return (pre, [])

      (_, C.If gdCmds l)          -> do
        branches <- forM gdCmds $ \(C.GdCmd guard body _) -> do
          wpStmts previous (toGuard (IF l) guard : pres) body post
        return (disjunct (map (toGuard (IF l)) (C.getGuards gdCmds)), branches)

      (Just (C.LoopInvariant p _ l), C.Do gdCmds _) -> do
        -- use the precondition of the loop as the postcondition of the branch
        branches <- forM gdCmds $ \(C.GdCmd guard body _) -> do
          let post' = conjunct (LoopInvariant p l : pres)
          wpStmts previous (toGuard (LOOP l) guard : pres) body post'

        return (LoopInvariant p l, branches)
      (_, C.Do _ l)               -> throwError (MissingAssertion l)
      (_, C.SpecQM l)             -> throwError (DigHole l)
      (_ , C.Spec _)              -> return (post, [])

assignmentEnv :: [Lower] -> [Expr] -> C.Subst
assignmentEnv xs es = Map.fromList (zip (map Left xs) es)

programToLasagna :: C.Program -> WPM Lasagna
programToLasagna (C.Program _ stmts _) = case (init stmts, last stmts) of
  (stmts', C.Assert p l) -> toLasagna stmts' (Assertion p l)
  (_     , stmt)         -> throwError (MissingPostcondition (locOf stmt))

programWP :: C.Program -> WPM Pred
programWP p = precond <$> programToLasagna p

-- Monad for calculating preconditions (for Lasagna)
type WPM = ExceptT StructError2 (State Int)

runWPM :: WPM a -> Either StructError2 a
runWPM f = evalState (runExceptT f) 0

instance C.Fresh WPM where
  fresh = do
    i <- get
    put (succ i)
    return i

--------------------------------------------------------------------------------
-- | Obligation

data Obligation2
  = Obligation Int Pred Pred ObliOrigin2
  deriving (Eq, Show, Generic)

instance ToNoLoc Obligation2 where
  toNoLoc (Obligation i p q o) =
    Obligation i (toNoLoc p) (toNoLoc q) (toNoLoc o)

data ObliOrigin2 = AroundAbort Loc
                | AroundSkip Loc
                | AssertGuaranteed Loc
                | AssertSufficient Loc
                | Assignment Loc
                | IfTotal Loc
                | LoopBase Loc
                | LoopTermBase Loc
                | LoopInitialize Loc
      deriving (Eq, Show, Generic)

instance ToNoLoc ObliOrigin2 where
  toNoLoc (AroundAbort _)       = AroundAbort NoLoc
  toNoLoc (AroundSkip _)        = AroundSkip NoLoc
  toNoLoc (AssertGuaranteed _)  = AssertGuaranteed NoLoc
  toNoLoc (AssertSufficient _)  = AssertSufficient NoLoc
  toNoLoc (Assignment _)        = Assignment NoLoc
  toNoLoc (IfTotal _)           = IfTotal NoLoc
  toNoLoc (LoopBase _)          = LoopBase NoLoc
  toNoLoc (LoopTermBase _)      = LoopTermBase NoLoc
  toNoLoc (LoopInitialize _)    = LoopInitialize NoLoc


-- Monad on top of WPM, for generating obligations
type ObliM = WriterT [Obligation2] (StateT Int WPM)

instance C.Fresh ObliM where
  fresh = do
    i <- get
    put (succ i)
    return i

runObliM :: ObliM a -> Either StructError2 (a, [Obligation2])
runObliM f = runWPM (evalStateT (runWriterT f) 0)

sweep :: C.Program -> Either StructError2 [Obligation2]
sweep program = fmap snd $ runObliM $ do
  lasagna <- lift (lift (programToLasagna program))
  genObli [] lasagna

tellObli :: [Pred] -> [Pred] -> ObliOrigin2 -> ObliM ()
tellObli ps qs l = do
  let p = conjunct ps
  let q = disjunct qs

  -- NOTE: this could use some love
  unless (C.predEq (toExpr p) (toExpr q)) $ do
    i <- get
    put (succ i)
    tell [Obligation i p q l]

conjunct :: [Pred] -> Pred
conjunct [] = Constant C.true
conjunct [x] = x
conjunct xs = Conjunct xs

disjunct :: [Pred] -> Pred
disjunct [] = Constant C.false
disjunct [x] = x
disjunct xs = Disjunct xs

genObli :: [Pred]   -- additional preconditions to be conjuncted with
        -> Lasagna
        -> ObliM ()
genObli _    (Final _) = return ()
genObli pres (Layer previousStmt stmtPreCond stmt branches stmts) = do

  -- traceShow ("stmtPreCond: " ++ show stmtPreCond) (return ())
  -- traceShow ("pres: " ++ show pres) (return ())
  -- traceShow ("stmt: " ++ show stmt) (return ())

  let post = [precond stmts]
  let pre = if null pres then [stmtPreCond] else stmtPreCond:pres

  case stmt of
    C.Abort l -> tellObli pre [Constant C.false] (AroundAbort l)

    C.Skip l -> tellObli pre post (AroundSkip l)

    C.Assert p l -> do
      tellObli pre [Assertion p l] (AssertGuaranteed l)
      tellObli [Assertion p l] post (AssertSufficient l)

    C.LoopInvariant _ _ _ -> return ()

    C.Assign xs es l -> do
      post' <- mapM (subst (assignmentEnv xs es)) post
      tellObli pre post' (Assignment l)

    C.If gdCmds l -> do

      -- NOTE: you don't see this because somehow both antecedent & consequent are the same
      tellObli pre (map (toGuard (IF l)) $ C.getGuards gdCmds) (IfTotal l)

      -- generate POs for each branch
      forM_ branches (genObli pre)


      -- generate POs for each branch
      -- forM_ gdCmds $ \(C.GdCmd _ body _) -> do
      --   body' <- lift $ lift $ toLasagna body (disjunct post)
      --   genObli pre body'

    C.Do gdCmds l -> case previousStmt of
      (Just (C.LoopInvariant _ bnd _)) -> do
        let guards = C.getGuards gdCmds

        -- base case
        tellObli
          (pre ++ map (Negate . toGuard (LOOP l)) guards)
          post
          (LoopBase l)

        -- inductive cases
        forM_ branches (genObli pre)
        -- forM_ gdCmds $ \(C.GdCmd guard body _) -> do
        --   -- the precondition of the loop as the postcondition of the branch
        --   let post' = conjunct pre
        --   body' <- lift $ lift $ toLasagna body post'
        --   genObli (pre ++ [toGuard (LOOP l) guard]) body'

        -- termination
        tellObli
          (pre ++ map (toGuard (LOOP l)) guards)
          [Bound $ bnd `C.gte` (C.Lit (C.Num 0) NoLoc)]
          (LoopTermBase l)

        -- bound decrementation
        oldBnd <- C.freshVar "bnd"
        forM_ gdCmds $ \(C.GdCmd guard body _) -> do
          let post' = Bound $ bnd `C.lt` C.Var oldBnd NoLoc
          body' <- lift $ lift $ toLasagna body post'
          genObli
            (pre ++
              [ toGuard (LOOP l) guard
              , Bound $ bnd `C.eqq` C.Var oldBnd NoLoc
              ])
           body'

        return ()


      _ -> throwError (MissingBound l)
       {- Or if we want to tolerate the user and carry on ---
       do -- warn that bnd is missing
        let gdCmds' = map (\(GdCmd x y _) -> (depart x, y)) gdCmds
        let guards = map fst gdCmds'
        obligate (inv `A.conj` (A.conjunct (map A.neg guards))) post
        --
        forM_ gdCmds' $ \(guard, body) -> do
          structStmts b (inv `A.conj` guard) Nothing body inv
       -}

    C.SpecQM l -> throwError $ DigHole l

    C.Spec _ -> return ()

  genObli pres stmts


--------------------------------------------------------------------------------
-- | Specification

data Specification2 = Specification
  { specID       :: Int
  , specPreCond  :: Pred
  , specPostCond :: Pred
  , specLoc      :: Loc
  } deriving (Eq, Show, Generic)

-- Monad on top of WPM, for generating specifications
type SpecM = WriterT [Specification2] (StateT Int WPM)

-- tellSpec :: Pred -> Pred -> Specification2 -> SpecM ()
-- tellSpec p q l = do
--   i <- get
--   put (succ i)
--   lift $ tell [Specification i p q l]

-- genSpec :: [Pred]   -- additional preconditions to be conjuncted with
--         -> Lasagna
--         -> SpecM ()
-- genSpec _    (Final _) = return ()
-- genSpec pres (Layer previousStmt stmtPreCond stmt stmts) = do
--   case stmtPreCond of
--     C.Spec l -> tellSpec pre post l

--------------------------------------------------------------------------------
-- | StructError

data StructError2 = MissingAssertion Loc
                 | MissingBound Loc
                 | ExcessBound  Loc
                 | MissingPostcondition Loc
                 | DigHole Loc
                deriving (Eq, Show, Generic)

instance Located StructError2 where
  locOf (MissingAssertion loc) = loc
  locOf (MissingBound     loc) = loc
  locOf (ExcessBound      loc) = loc
  locOf (MissingPostcondition loc) = loc
  locOf (DigHole loc) = loc

instance ToJSON StructError2 where


--------------------------------------------------------------------------------
-- | Rose tree of preconditions, for testing

-- data WPTree = WPLayer Pred [WPTree] WPTree | WPFinal Pred

  -- case toWPTree xs of
  -- Leaf q -> case branches of
  --             [] -> Node [Leaf p, Leaf q]
  --             _  -> Node [Node p branches, Leaf q]
  -- Node q xs -> case branches of
  --               [] -> Node [Leaf p, Leaf q]
  --               _  -> Node [Node p branches, Leaf q]

  -- WPLayer p (map toWPTree branches) (toWPTree xs)
-- toWPTree (Final p) = WPFinal p
-- toWPTree (Layer _ p _ branches xs) = WPLayer p (map toWPTree branches) (toWPTree xs)
