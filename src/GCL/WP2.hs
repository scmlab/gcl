{-# LANGUAGE OverloadedStrings, FlexibleContexts, DeriveGeneric,
             TypeSynonymInstances, FlexibleInstances #-}

module GCL.WP2 where

import Control.Monad.State hiding (guard)
import Control.Monad.Writer hiding (guard)
import Control.Monad.Except hiding (guard)

import qualified Data.Map as Map
import Data.Loc (Loc(..), Located(..))
import Data.Aeson
import Data.List (break)
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


-- --------------------------------------------------------------------------------
-- -- | Lasagna, alternating sequence of Pred & Statement
--
-- type PreviousStmt = Maybe Stmt
-- type ImposedConds = [Pred]
--
-- data Lasagna = Layer
--                   ImposedConds  -- preconditions imposed on the current statement
--                   Pred          -- precondition of the current statement
--                   Stmt          -- the current statement
--                   [Lasagna]     -- sub-lasagne of the current statement (IF, LOOP)
--                   Lasagna       -- following layers
--              | Final Pred
--              deriving (Show)
--
--
--
-- -- access the precondition of a Lasagna
-- precond :: Lasagna -> Pred
-- precond (Final p) = p
-- precond (Layer _ p _ _ _) = p
--
--
-- toLasagna :: [Stmt] -> Pred -> WPM Lasagna
-- toLasagna = wpStmts Nothing []
--   where
--
--     -- the workhorse
--     wpStmts :: PreviousStmt -> ImposedConds -> [Stmt] -> Pred -> WPM Lasagna
--     wpStmts _        _       []     post = return $ Final post
--     wpStmts previous imposed (x:xs) post = do
--
--       -- the preconditions only affect the current statement
--       -- they should not be imposed on the rest of the statements
--       xs' <- wpStmts (Just x) [] xs post
--
--       -- see if the previous statement is an assertion
--       -- if so, then we add it to the list of imposed conditions
--       imposed' <- case previous of
--         Just (C.Assert        p   l) -> return $ Assertion     p l : imposed
--         Just (C.LoopInvariant p _ l) -> return $ LoopInvariant p l : imposed
--         _ -> case x of
--               C.Do _ l -> throwError (MissingAssertion l)
--               _        -> return imposed
--
--       -- calculate the precondition of the current statement
--       let post' = precond xs'
--       (pre, branches) <- wp imposed' x post'
--
--       return $ Layer imposed pre x branches xs'
--
--     -- calculates the weakest precondition of a given statement
--     -- along with the imposed preconditions
--     wp :: ImposedConds -> Stmt -> Pred -> WPM (Pred, [Lasagna])
--     wp imposed current post = do
--
--       -- override the returned precondition when there are imposed preconditions
--       let conjectedImposed = conjunct (reverse imposed)
--       let override p = if null imposed
--                         then p
--                         else conjectedImposed
--
--       case current of
--         C.Abort _              -> return (override (Constant C.false), [])
--         C.Skip _               -> return (override post, [])
--         C.Assert p l           -> return (Assertion p l, [])
--         C.LoopInvariant p _ l  -> return (LoopInvariant p l, [])
--         C.Assign xs es _       -> do
--           pre <- subst (assignmentEnv xs es) post
--           return (override pre, [])
--
--         C.If gdCmds l -> do
--           branches <- forM gdCmds $ \(C.GdCmd guard body _) -> do
--             let imposed' = toGuard (IF l) guard : imposed
--             wpStmts Nothing imposed' body post
--
--           return (disjunct (map (toGuard (IF l)) (C.getGuards gdCmds)), branches)
--
--         C.Do gdCmds l -> do
--           -- use the precondition of the loop as the postcondition of the branch
--           branches <- forM gdCmds $ \(C.GdCmd guard body _) -> do
--             let imposed' = toGuard (LOOP l) guard : imposed
--             wpStmts Nothing imposed' body conjectedImposed
--
--           return (conjectedImposed, branches)
--
--         C.SpecQM l            -> throwError (DigHole l)
--         C.Spec _              -> return (override post, [])

assignmentEnv :: [Lower] -> [Expr] -> C.Subst
assignmentEnv xs es = Map.fromList (zip (map Left xs) es)
--
-- programToLasagna :: C.Program -> WPM Lasagna
-- programToLasagna (C.Program _ stmts _) = case (init stmts, last stmts) of
--   (stmts', C.Assert p l) -> toLasagna stmts' (Assertion p l)
--   (_     , stmt)         -> throwError (MissingPostcondition (locOf stmt))
--
-- programWP :: C.Program -> WPM Pred
-- programWP p = precond <$> programToLasagna p
--
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

data PO
  = PO Int Pred Pred Origin
  deriving (Eq, Show, Generic)

instance ToNoLoc PO where
  toNoLoc (PO i p q o) =
    PO i (toNoLoc p) (toNoLoc q) (toNoLoc o)

data Origin = AtAbort         Loc
            | AtSkip          Loc
            | AtSpec          Loc
            | AtAssignment    Loc
            | AtAssertion     Loc -- AssertSufficient
            | AtLoopInvariant Loc
            | AtIf            Loc
            | AtLoop          Loc

            | AssertGuaranteed Loc
            | AssertSufficient Loc
            | Assignment Loc
            | IfTotal Loc
            | LoopBase Loc
            | LoopTermBase Loc
            | LoopInitialize Loc
            deriving (Eq, Show, Generic)

instance ToNoLoc Origin where
  toNoLoc (AtAbort          _)  = AtAbort NoLoc
  toNoLoc (AtSkip           _)  = AtSkip NoLoc
  toNoLoc (AtSpec           _)  = AtSpec NoLoc
  toNoLoc (AtAssignment     _)  = AtAssignment NoLoc
  toNoLoc (AtAssertion      _)  = AtAssertion NoLoc
  toNoLoc (AtLoopInvariant  _)  = AtLoopInvariant NoLoc
  toNoLoc (AtIf             _)  = AtIf NoLoc
  toNoLoc (AtLoop           _)  = AtLoop NoLoc

  toNoLoc (AssertGuaranteed _)  = AssertGuaranteed NoLoc
  toNoLoc (AssertSufficient _)  = AssertSufficient NoLoc
  toNoLoc (Assignment _)        = Assignment NoLoc
  toNoLoc (IfTotal _)           = IfTotal NoLoc
  toNoLoc (LoopBase _)          = LoopBase NoLoc
  toNoLoc (LoopTermBase _)      = LoopTermBase NoLoc
  toNoLoc (LoopInitialize _)    = LoopInitialize NoLoc

toOrigin :: C.Stmt -> Origin
toOrigin (C.Abort             l) = AtAbort l
toOrigin (C.Skip              l) = AtSkip l
toOrigin (C.Assert          _ l) = AtAssertion l
toOrigin (C.LoopInvariant _ _ l) = AtLoopInvariant l
toOrigin (C.Assign        _ _ l) = AtAssignment l
toOrigin (C.If              _ l) = AtIf l
toOrigin (C.Do              _ l) = AtLoop l
toOrigin (C.SpecQM            l) = AtSpec l
toOrigin (C.Spec              l) = AtSpec l

-- get the Origin of the first statement in a Struct
structToOrigin :: Struct -> Origin
structToOrigin (Struct _ [] next) = AtAssertion (locOf $ precond next)
structToOrigin (Struct _ (Line _ stmt:_) _) = toOrigin stmt
structToOrigin (Struct _ (Block _ sort _:_) _) = sortToOgirin sort
structToOrigin (Postcond p) = AtAssertion (locOf p)

sortToOgirin :: Sort -> Origin
sortToOgirin (IF l) = AtIf l
sortToOgirin (LOOP l) = AtLoop l

-- Monad on top of WPM, for generating obligations
type POM = WriterT [PO] (StateT Int WPM)

instance C.Fresh POM where
  fresh = do
    i <- get
    put (succ i)
    return i

runPOM :: POM a -> Either StructError2 (a, [PO])
runPOM f = runWPM (evalStateT (runWriterT f) 0)

sweep :: C.Program -> Either StructError2 [PO]
sweep program = fmap snd $ runPOM $ do
  struct <- lift (lift (programToStruct program))
  genPO struct

tellPO :: Pred -> Pred -> Origin -> POM ()
tellPO p q l = do
  -- let p = conjunct ps
  -- let q = disjunct qs

  -- NOTE: this could use some love
  unless (C.predEq (toExpr p) (toExpr q)) $ do
    i <- get
    put (succ i)
    tell [PO i p q l]

conjunct :: [Pred] -> Pred
conjunct [] = Constant C.true
conjunct [x] = x
conjunct xs = Conjunct xs

disjunct :: [Pred] -> Pred
disjunct [] = Constant C.false
disjunct [x] = x
disjunct xs = Disjunct xs

genPO :: Struct -> POM ()
genPO struct = do
  let origin = structToOrigin struct
  case struct of
    Struct pre [] next -> do
      tellPO pre (precond next) origin
      genPO next
    Struct pre (Line p stmt:_) next -> do
      tellPO pre p origin
      genPO next
    Struct pre (Block p sort xs:_) next -> do
      tellPO pre p origin
      mapM_ genPO xs
      genPO next
    Postcond _ -> return ()


-- genPO (Final _) = return ()
-- genPO (Layer previousStmt stmtPreCond stmt branches stmts) = return ()



-- genObli :: [Pred]   -- imposed preconditions
--         -> Lasagna
--         -> POM ()
-- genObli _    (Final _) = return ()
-- genObli pres (Layer previousStmt stmtPreCond stmt branches stmts) = do
--
--   -- traceShow ("stmtPreCond: " ++ show stmtPreCond) (return ())
--   -- traceShow ("pres: " ++ show pres) (return ())
--   -- traceShow ("stmt: " ++ show stmt) (return ())
--
--   let post = [precond stmts]
--
--   let pre = if null pres    -- if no imposed preconditions
--               then [stmtPreCond]  -- use the inferred precondition of `stmt`
--               else pres           -- else use the imposed preconditions
--
--   case stmt of
--     C.Abort l -> tellObli pre [Constant C.false] (AtAbort l)
--
--     C.Skip l -> tellObli pre post (AtSkip l)
--
--     C.Assert p l -> do
--       tellObli pre [Assertion p l] (AssertGuaranteed l)
--       tellObli [Assertion p l] post (AssertSufficient l)
--
--     C.LoopInvariant _ _ _ -> return ()
--
--     C.Assign xs es l -> do
--       post' <- mapM (subst (assignmentEnv xs es)) post
--       tellObli pre post' (Assignment l)
--
--     C.If gdCmds l -> do
--
--       tellObli pre (map (toGuard (IF l)) $ C.getGuards gdCmds) (IfTotal l)
--
--       -- generate POs for each branch
--       forM_ branches (genObli pre)
--
--       -- let guards = C.getGuards gdCmds
--       -- let zipped = zip guards branches
--       --
--       -- forM_ zipped $ \(guard, branch) -> do
--       --   genObli (toGuard (IF l) guard : pres) branch
--
--     C.Do gdCmds l -> case previousStmt of
--       (Just (C.LoopxInvariant _ bnd _)) -> do
--         let guards = C.getGuards gdCmds
--
--         -- base case
--         tellObli
--           (pre ++ map (Negate . toGuard (LOOP l)) guards)
--           post
--           (LoopBase l)
--
--         -- inductive cases
--         forM_ branches (genObli pre)
--
--         -- termination
--         tellObli
--           (pre ++ map (toGuard (LOOP l)) guards)
--           [Bound $ bnd `C.gte` (C.Lit (C.Num 0) NoLoc)]
--           (LoopTermBase l)
--
--         -- bound decrementation
--         oldBnd <- C.freshVar "bnd"
--         forM_ gdCmds $ \(C.GdCmd guard body _) -> do
--           let post' = Bound $ bnd `C.lt` C.Var oldBnd NoLoc
--           body' <- lift $ lift $ toLasagna body post'
--           genObli
--             (pre ++
--               [ toGuard (LOOP l) guard
--               , Bound $ bnd `C.eqq` C.Var oldBnd NoLoc
--               ])
--            body'
--
--         return ()
--
--
--       _ -> throwError (MissingBound l)
--        {- Or if we want to tolerate the user and carry on ---
--        do -- warn that bnd is missing
--         let gdCmds' = map (\(GdCmd x y _) -> (depart x, y)) gdCmds
--         let guards = map fst gdCmds'
--         obligate (inv `A.conj` (A.conjunct (map A.neg guards))) post
--         --
--         forM_ gdCmds' $ \(guard, body) -> do
--           structStmts b (inv `A.conj` guard) Nothing body inv
--        -}
--
--     C.SpecQM l -> throwError $ DigHole l
--
--     C.Spec _ -> return ()
--
--   genObli pres stmts


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


--------------------------------------------------------------------------------
-- | StructError

data StructError2 = MissingAssertion Loc
                 | MissingBound Loc
                 | ExcessBound  Loc
                 | MissingPrecondition Loc
                 | MissingPostcondition Loc
                 | DigHole Loc
                deriving (Eq, Show, Generic)

instance Located StructError2 where
  locOf (MissingAssertion loc) = loc
  locOf (MissingBound     loc) = loc
  locOf (ExcessBound      loc) = loc
  locOf (MissingPrecondition loc) = loc
  locOf (MissingPostcondition loc) = loc
  locOf (DigHole loc) = loc

instance ToJSON StructError2 where

--------------------------------------------------------------------------------
-- | Struct


data Struct = Struct Pred [Line] Struct
            | Postcond Pred
            deriving (Eq)

data Line = Line  Pred C.Stmt
          | Block Pred Sort [Struct]


-- igonring Stmt in Lines and Sort in Blocks
instance Eq Line where
  Line p _    == Line q _     = p == q
  Block p _ _ == Block q _ _  = p == q
  _           == _            = False

-- For wpStmts'
data Accum = Accum [Line] Struct

precond :: Struct -> Pred
precond (Struct p _ _) = p
precond (Postcond p)   = p

toStruct :: Pred -> Accum -> Struct
toStruct pre (Accum xs next) = Struct pre xs next

wpStmts :: [Pred] -> [Stmt] -> Pred -> WPM Struct
wpStmts imposed stmts post = do
  accum <- wpStmts' imposed stmts post
  return $ toStruct (conjunct $ reverse imposed) accum
  where


wpStmts' :: [Pred] -> [Stmt] -> Pred -> WPM Accum
wpStmts' _       []           post = return (Accum [] (Postcond post))
wpStmts' imposed (stmt:stmts) post = case stmt of
  C.Assert p l -> do
    accum <- wpStmts' [Assertion p l]     stmts post
    return (Accum [] (toStruct (Assertion p l) accum))
  C.LoopInvariant p _ l -> do
    accum <- wpStmts' [LoopInvariant p l] stmts post
    return (Accum [] (toStruct (LoopInvariant p l) accum))
  otherStmt             -> do
    xs <- wpStmts' []                  stmts post
    x <- wp imposed otherStmt (precondAccum xs)
    return $ insert x xs

  where
    insert :: Line -> Accum -> Accum
    insert x (Accum xs ys) = Accum (x:xs) ys

    precondAccum :: Accum -> Pred
    precondAccum (Accum [] xs) = precond xs
    precondAccum (Accum (Line  p _  :_) _) = p
    precondAccum (Accum (Block p _ _:_) _) = p

wp :: [Pred] -> Stmt -> Pred -> WPM Line
wp imposed stmt post = case stmt of
  C.Abort _              -> return $ Line (Constant C.false)  stmt
  C.Skip _               -> return $ Line post                stmt
  C.Assert p l           -> return $ Line (Assertion     p l) stmt
  C.LoopInvariant p _ l  -> return $ Line (LoopInvariant p l) stmt
  C.Assign xs es _       -> do
    pre <- subst (assignmentEnv xs es) post
    return $ Line pre stmt

  C.If gdCmds l -> do
    structs <- forM gdCmds $ \(C.GdCmd guard body _) -> do
      let imposed' = toGuard (IF l) guard : imposed
      wpStmts imposed' body post

    return $ Block
              (disjunct (map (toGuard (IF l)) (C.getGuards gdCmds)))
              (IF l)
              structs

  C.Do gdCmds l -> if null imposed
    then throwError (MissingAssertion l)
    else do
    -- use the loop invariant as the postcondition of the branch
    structs <- forM gdCmds $ \(C.GdCmd guard body _) -> do
      let imposed' = toGuard (LOOP l) guard : imposed
      wpStmts imposed' body (conjunct $ reverse imposed)

    return $ Block (conjunct $ reverse imposed) (LOOP l) structs

  C.SpecQM l            -> throwError (DigHole l)
  C.Spec _              -> return $ Line post stmt


programToStruct :: C.Program -> WPM Struct
programToStruct (C.Program _ stmts _) = case (init stmts, last stmts) of
  (C.Assert          p l:stmts', C.Assert q m) -> wpStmts [Assertion p l] stmts' (Assertion q m)
  (C.LoopInvariant p _ l:stmts', C.Assert q m) -> wpStmts [LoopInvariant p l] stmts' (Assertion q m)
  ([]                          , C.Assert _ l) -> throwError (MissingPrecondition l)
  (others               :_     , C.Assert _ _) -> throwError (MissingPrecondition (locOf others))
  (_                           , stmt)         -> throwError (MissingPostcondition (locOf stmt))
