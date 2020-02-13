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


--------------------------------------------------------------------------------
-- | Lasagna, alternating sequence of Pred & Statement

type PreviousStmt = Maybe Stmt
data Lasagna = Layer
                  PreviousStmt    -- the previous statement
                  Pred            -- precondition
                  Stmt            -- the current statement
                  Lasagna         -- following layers
             | Final Pred
             deriving (Show)

-- access the precondition of a Lasagna
precond :: Lasagna -> Pred
precond (Final p) = p
precond (Layer _ p _ _) = p


toLasagna :: [Stmt] -> Pred -> WPM Lasagna
toLasagna = toLasagna' Nothing
  where

    -- the workhorse
    toLasagna' :: PreviousStmt -> [Stmt] -> Pred -> WPM Lasagna
    toLasagna' _        []     post = return $ Final post
    toLasagna' previous (x:xs) post = do
      xs' <- toLasagna' (Just x) xs post
      pre <- wp previous x (precond xs')
      return $ Layer previous pre x xs'

    -- calculates the weakest precondition of a given statement
    -- along with the postcondition and its previous statement
    wp :: PreviousStmt -> Stmt -> Pred -> WPM Pred
    wp previous current post = case (previous, current) of
      (_, C.Abort _)              -> return (Constant C.false)
      (_, C.Skip _)               -> return post
      (_, C.Assert p l)           -> return (Assertion p l)
      (_, C.LoopInvariant p _ l)  -> return (LoopInvariant p l)
      (_, C.Assign xs es _)       -> subst (assignmentEnv xs es) post
      (_, C.If gdCmds l)          ->
        return (Disjunct (map (toGuard (IF l)) (C.getGuards gdCmds)))
      (Just (C.LoopInvariant p _ l), C.Do _ _) -> return (LoopInvariant p l)
      (_, C.Do _ l)               -> throwError (MissingAssertion l)
      (_, C.SpecQM l)             -> throwError (DigHole l)
      (_ , C.Spec _)              -> return post

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

tellObli :: Pred -> Pred -> ObliOrigin2 -> ObliM ()
tellObli p q l = do
  -- NOTE: this could use some love
  unless (C.predEq (toExpr p) (toExpr q)) $ do
    i <- get
    put (succ i)
    tell [Obligation i p q l]

genObli :: [Pred]   -- additional preconditions to be conjuncted with
        -> Lasagna
        -> ObliM ()
genObli _    (Final _) = return ()
genObli pres (Layer previousStmt stmtPreCond stmt stmts) = do
  let post = precond stmts
  let pre = if null pres then stmtPreCond else Conjunct (stmtPreCond:pres)

  case stmt of
    C.Abort l -> tellObli pre (Constant C.false) (AroundAbort l)

    C.Skip l -> tellObli pre post (AroundSkip l)

    C.Assert p l -> do
      tellObli pre (Assertion p l) (AssertGuaranteed l)
      tellObli (Assertion p l) post (AssertSufficient l)

    C.LoopInvariant _ _ _ -> return ()

    C.Assign xs es l -> do
      post' <- subst (assignmentEnv xs es) post
      tellObli pre post' (Assignment l)

    C.If gdCmds l -> do
      tellObli pre (Disjunct $ map (toGuard (IF l)) $ C.getGuards gdCmds) (IfTotal l)

      -- generate POs for each branch
      forM_ gdCmds $ \(C.GdCmd guard body _) -> do
        body' <- lift $ lift $ toLasagna body post
        genObli [pre, toGuard (IF l) guard] body'

    C.Do gdCmds l -> case previousStmt of
      (Just (C.LoopInvariant _ bnd _)) -> do
        let guards = C.getGuards gdCmds

        -- base case
        tellObli
          (Conjunct $ pre : map (Negate . toGuard (LOOP l)) guards)
          post
          (LoopBase l)

        -- inductive cases
        forM_ gdCmds $ \(C.GdCmd guard body _) -> do
          -- the precondition of the loop as the postcondition of the branch
          let post' = pre
          body' <- lift $ lift $ toLasagna body post'
          genObli [pre, toGuard (LOOP l) guard] body'

        -- termination
        tellObli
          (Conjunct $ pre : map (toGuard (LOOP l)) guards)
          (Bound $ bnd `C.gte` (C.Lit (C.Num 0) NoLoc))
          (LoopBase l)

        -- bound decrementation
        oldBnd <- C.freshVar "bnd"
        forM_ gdCmds $ \(C.GdCmd guard body _) -> do
          let post' = Bound $ bnd `C.lt` C.Var oldBnd NoLoc
          body' <- lift $ lift $ toLasagna body post'
          genObli
           [ pre
           , toGuard (LOOP l) guard
           , Bound $ bnd `C.eqq` C.Var oldBnd NoLoc
           ]
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
