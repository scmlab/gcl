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

import Pretty.Concrete ()
import Pretty.Predicate ()
import Debug.Trace (traceShow)
import Data.Text.Prettyprint.Doc


--------------------------------------------------------------------------------
-- | Lasagna, alternating sequence of Pred & Statement

type PreviousStmt = Maybe Stmt
type ImposedConds = [Pred]

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
    wpStmts :: PreviousStmt -> ImposedConds -> [Stmt] -> Pred -> WPM Lasagna
    wpStmts _        _       []     post = return $ Final post
    wpStmts previous imposed (x:xs) post = do

      -- the preconditions only affect the current statement
      -- they should not be imposed on the rest of the statements
      xs' <- wpStmts (Just x) [] xs post

      -- see if the previous statement is an assertion
      -- if so, then we add it to the list of imposed conditions
      imposed' <- case previous of
        Just (C.Assert        p   l) -> return $ Assertion     p l : imposed
        Just (C.LoopInvariant p _ l) -> return $ LoopInvariant p l : imposed
        _ -> case x of
              C.Do _ l -> throwError (MissingAssertion l)
              _        -> return imposed

      -- calculate the precondition of the current statement
      let post' = precond xs'
      (pre, branches) <- wp imposed' x post'

      return $ Layer previous pre x branches xs'

    -- calculates the weakest precondition of a given statement
    -- along with the imposed preconditions
    wp :: ImposedConds -> Stmt -> Pred -> WPM (Pred, [Lasagna])
    wp imposed current post = do

      -- traceShow ("") (return ())
      -- traceShow ("current: " ++ show (pretty current)) (return ())
      -- traceShow ("imposed: " ++ show (prettyList imposed)) (return ())

      let pre = if null imposed
                    then post
                    else conjunct imposed

      case current of
        C.Abort _              -> return (Constant C.false, [])
        C.Skip _               -> return (pre, [])
        C.Assert p l           -> return (Assertion p l, [])
        C.LoopInvariant p _ l  -> return (LoopInvariant p l, [])
        C.Assign xs es _       -> do
          pre' <- subst (assignmentEnv xs es) pre
          return (pre', [])

        -- C.If gdCmds l1 -> do
        --   branches <- forM gdCmds $ \(C.GdCmd guard body _) -> do
        --     wpStmts previous (Assertion p l0 : toGuard (IF l1) guard : pres) body post
        --   return (Assertion p l0, branches)
          -- return (disjunct (map (toGuard (IF l1)) (C.getGuards gdCmds)), branches)

        C.If gdCmds l -> do
          branches <- forM gdCmds $ \(C.GdCmd guard body _) -> do
            let imposed' = toGuard (LOOP l) guard : imposed
            wpStmts Nothing imposed' body post

          return (disjunct (map (toGuard (IF l)) (C.getGuards gdCmds)), branches)

        C.Do gdCmds l -> do
          let imposedConjunct = conjunct imposed
          -- use the precondition of the loop as the postcondition of the branch
          branches <- forM gdCmds $ \(C.GdCmd guard body _) -> do
            let imposed' = toGuard (LOOP l) guard : imposed
            wpStmts Nothing imposed' body imposedConjunct

          return (imposedConjunct, branches)

        C.SpecQM l            -> throwError (DigHole l)
        C.Spec _              -> return (pre, [])

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

genObli :: [Pred]   -- imposed preconditions
        -> Lasagna
        -> ObliM ()
genObli _    (Final _) = return ()
genObli pres (Layer previousStmt stmtPreCond stmt branches stmts) = do

  -- traceShow ("stmtPreCond: " ++ show stmtPreCond) (return ())
  -- traceShow ("pres: " ++ show pres) (return ())
  -- traceShow ("stmt: " ++ show stmt) (return ())

  let post = [precond stmts]

  let pre = if null pres    -- if no imposed preconditions
              then [stmtPreCond]  -- use the inferred precondition of `stmt`
              else pres           -- else use the imposed preconditions

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

      tellObli pre (map (toGuard (IF l)) $ C.getGuards gdCmds) (IfTotal l)

      -- generate POs for each branch
      forM_ branches (genObli pre)

      -- let guards = C.getGuards gdCmds
      -- let zipped = zip guards branches
      --
      -- forM_ zipped $ \(guard, branch) -> do
      --   genObli (toGuard (IF l) guard : pres) branch

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
-- |

-- data Line = Line Pred Stmt              -- with a precondition
--           | Free Pred Stmt [FreeBlk]    -- with a common, inferred precondition
--           | Imposed   Stmt [ImposedBlk]
--           deriving (Show, Eq)
--
-- data ImposedBlk = ImposedBlk Pred [Line] Pred -- with imposed precondition
--                 deriving (Show, Eq)
--
-- data FreeBlk = FreeBlk [Line] Pred
--              deriving (Show, Eq)

-- data Block = Imposed Pred [Line] Pred -- with imposed precondition
--            | Free         [Line] Pred
--           deriving (Show, Eq)


-- linePrecond :: Line -> Pred
-- linePrecond (Line  p _) = p
-- linePrecond (Block p _) = p
--
-- blockPrecond :: Block -> Pred
-- blockPrecond (Imposed _ []     p) = p
-- blockPrecond (Imposed _ (x:xs) _) = linePrecond x
-- blockPrecond (Free      []     p) = p
-- blockPrecond (Free      (x:xs) _) = linePrecond x
--
-- wpStmts :: Maybe Pred -> [Stmt] -> Pred -> WPM Block
-- wpStmts Nothing    []     post = Free    []  post
-- wpStmts Nothing    (x:xs) post = do


  -- let xs' = case wpStmts Nothing xs post of
  --           Imposed _ ls _ -> ls
  --           Free      ls _ -> ls
  -- x' <- wp x
  -- return $ Free
-- wpStmts (Just pre) []     post = Imposed pre post

    -- wpStmts (x:xs) post = do
    --   xs' <- wpStmts (Just x) pres xs post
    --   (pre, branches) <- wp previous pres x (precond xs')
    --   return $ Layer previous pre x branches xs'
