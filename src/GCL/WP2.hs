{-# LANGUAGE OverloadedStrings, FlexibleContexts, DeriveGeneric,
             TypeSynonymInstances, FlexibleInstances #-}
{-# OPTIONS_GHC -fno-warn-name-shadowing #-}

module GCL.WP2 where

import Control.Monad.State hiding (guard)
import Control.Monad.Writer hiding (guard)
import Control.Monad.Except hiding (guard)

import qualified Data.Map as Map
import Data.Loc (Loc(..), Located(..), L(..), unLoc)
import Data.Aeson
import GHC.Generics

import Syntax.Concrete (Expr, Lower)
import qualified Syntax.Concrete as C
-- import qualified Syntax.Predicate as P
import Syntax.Predicate
import Syntax.Location (ToNoLoc(..))

import Pretty.Concrete ()
import Pretty.Predicate ()

assignmentEnv :: [Lower] -> [Expr] -> C.Subst
assignmentEnv xs es = Map.fromList (zip (map Left xs) es)

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

data Origin = AtAbort           Loc
            | AtSkip            Loc
            | AtSpec            Loc
            | AtAssignment      Loc
            | AtAssertion       Loc -- AssertSufficient
            | AtLoopInvariant   Loc
            | AtIf              Loc
            | AtLoop            Loc
            | AtTermination     Loc
            | AtBoundDecrement  Loc

            -- | AssertGuaranteed Loc
            -- | AssertSufficient Loc
            -- | Assignment Loc
            -- | IfTotal Loc
            -- | LoopBase Loc
            -- | LoopTermBase Loc
            -- | LoopInitialize Loc
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
  toNoLoc (AtTermination    _)  = AtTermination NoLoc
  toNoLoc (AtBoundDecrement _)  = AtBoundDecrement NoLoc

  -- toNoLoc (AssertGuaranteed _)  = AssertGuaranteed NoLoc
  -- toNoLoc (AssertSufficient _)  = AssertSufficient NoLoc
  -- toNoLoc (Assignment _)        = Assignment NoLoc
  -- toNoLoc (IfTotal _)           = IfTotal NoLoc
  -- toNoLoc (LoopBase _)          = LoopBase NoLoc
  -- toNoLoc (LoopInitialize _)    = LoopInitialize NoLoc

originOfStmt :: Stmt -> Origin
originOfStmt (Abort  l) = AtAbort (locOf l)
originOfStmt (Skip   l) = AtSkip (locOf l)
originOfStmt (Assign l _ _) = AtAssignment (locOf l)
originOfStmt (If l _  ) = AtIf (locOf l)
originOfStmt (Do l _ _) = AtLoop (locOf l)
originOfStmt (Spec   l) = AtSpec (locOf l)

-- get the Origin of the first statement in a Struct
originOfStruct :: Struct -> Origin
originOfStruct (Struct _ []       next) = AtAssertion (locOf $ precondStruct next)
originOfStruct (Struct _ (stmt:_)    _) = originOfStmt stmt
originOfStruct (Postcond p)             = AtAssertion (locOf p)

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
genPO (Postcond _) = return ()
genPO (Struct pre [] next) = do
  tellPO pre (precondStruct next) (AtAssertion (locOf $ precondStruct next))
  genPO next
genPO (Struct pre (stmt:stmts) next) = do
  tellPO pre (precond stmt) (originOfStmt stmt)

  case stmt of
    If _ gdCmds -> do
      -- inductive case
      mapM_ (genPO . gdCmdBody) gdCmds
    Do l bnd gdCmds -> do
      let loc = locOf l
      let guards = map gdCmdGuard gdCmds
      -- the loop invariant has already been stored in Struct
      let loopInvariant = pre
      -- post condition of the current DO statement
      let post = case stmts of
                  []    -> precondStruct next
                  (x:_) -> precond x

      -- base case
      tellPO
        (conjunct (loopInvariant : map Negate guards))
        post
        (AtLoop loc)

      -- inductive case
      mapM_ (genPO . gdCmdBody) gdCmds

      -- termination
      bndVar <- C.Var <$> C.freshVar "bnd" <*> pure NoLoc
      tellPO
        (conjunct (loopInvariant : guards))
        (Bound (bndVar `C.gte` C.number 0) NoLoc)
        (AtTermination loc)

      -- bound decrement
      forM_ gdCmds $ \(GdCmd _ body) -> do
        let start = Bound (bnd `C.eqq` bndVar) loc
        let pre = [start, precondStruct body]
        let post = Bound (bnd `C.lt` bndVar) NoLoc

        body' <- lift $ lift $ resetStruct pre body post

        genPO body'

    _ -> return ()

  genPO next

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
                 | MissingLoopInvariant Loc
                 | MissingBound Loc
                 | ExcessBound  Loc
                 | MissingPrecondition Loc
                 | MissingPostcondition Loc
                 | DigHole Loc
                deriving (Eq, Show, Generic)

instance Located StructError2 where
  locOf (MissingAssertion loc) = loc
  locOf (MissingLoopInvariant loc) = loc
  locOf (MissingBound     loc) = loc
  locOf (ExcessBound      loc) = loc
  locOf (MissingPrecondition loc) = loc
  locOf (MissingPostcondition loc) = loc
  locOf (DigHole loc) = loc

instance ToJSON StructError2 where

--------------------------------------------------------------------------------
-- | Struct


data Struct = Struct Pred [Stmt] Struct
            | Postcond Pred
            deriving (Eq)

data Stmt
  = Skip   (L Pred)
  | Abort  (L Pred)
  | Assign (L Pred) [Lower] [Expr]
  | Do     (L Pred) Expr [GdCmd]
  | If     (L Pred)      [GdCmd]
  | Spec   (L Pred)

data GdCmd = GdCmd
  { gdCmdGuard :: Pred
  , gdCmdBody :: Struct
  }
  deriving (Eq)

-- comparing only the constructor and the predicate
instance Eq Stmt where
  Skip l == Skip m = l == m
  Abort l == Abort m = l == m
  Assign l _ _ == Assign m _ _ = l == m
  Do l _ xs == Do m _ ys = l == m && xs == ys
  If l xs == If m ys = l == m && xs == ys
  Spec l == Spec m = l == m
  _ == _ = False

-- For wpStmts'
data Accum = Accum [Stmt] Struct

precondStruct :: Struct -> Pred
precondStruct (Struct p _ _) = p
precondStruct (Postcond p)   = p

precond :: Stmt -> Pred
precond (Skip   l    ) = unLoc l
precond (Abort  l    ) = unLoc l
precond (Assign l _ _) = unLoc l
precond (Do     l _ _) = unLoc l
precond (If     l _  ) = unLoc l
precond (Spec   l    ) = unLoc l

toStruct :: Pred -> Accum -> Struct
toStruct pre (Accum xs next) = Struct pre xs next

-- reset the stored preconditions and postconditions of a struct
resetStruct :: [Pred] -> Struct -> Pred -> WPM Struct
resetStruct _       (Postcond _dumped)          post = return $ Postcond post
resetStruct imposed (Struct _dumped stmts next) post = do
  next' <- resetStruct [] next post
  accum <- wpStmts'' imposed stmts next'
  return $ toStruct (conjunct $ reverse imposed) accum

  where
    wpStmts'' :: [Pred] -> [Stmt] -> Struct -> WPM Accum
    wpStmts'' _       []           next = return $ Accum [] next
    wpStmts'' imposed (stmt:stmts) next = do
      xs <- wpStmts'' [] stmts next
      x <- wp'' imposed stmt (precondAccum xs)
      return $ insert x xs

    wp'' :: [Pred] -> Stmt -> Pred -> WPM Stmt
    wp'' imposed stmt post = case stmt of
      Skip l -> return $ Skip (L (locOf l) post)
      Abort l -> return $ Abort l
      Assign l xs es -> do
        pre <- subst (assignmentEnv xs es) post
        return $ Assign (L (locOf l) pre) xs es
      If l gdCmds -> do
        gdCmds' <- forM gdCmds $ \(GdCmd guard body) -> do
          let imposed' = guard : imposed
          body' <- resetStruct imposed' body post
          return $ GdCmd guard body'
        return $ If l gdCmds'
      Do (L l inv) bnd gdCmds -> do
        gdCmds' <- forM gdCmds $ \(GdCmd guard body) -> do
          let imposed' = guard : imposed
          body' <- resetStruct imposed' body inv
          return $ GdCmd guard body'

        return $ Do (L l inv) bnd gdCmds'
      Spec l -> return $ Spec (L (locOf l) post)

wpStmts :: [Pred] -> [C.Stmt] -> Pred -> WPM Struct
wpStmts imposed stmts post = do
  accum <- wpStmts' imposed stmts post
  return $ toStruct (conjunct $ reverse imposed) accum
  where


wpStmts' :: [Pred] -> [C.Stmt] -> Pred -> WPM Accum
wpStmts' _       []           post = return (Accum [] (Postcond post))
wpStmts' imposed (stmt:stmts) post = case stmt of
  C.Assert p l -> do
    accum <- wpStmts' [Assertion p l]     stmts post
    return (Accum [] (toStruct (Assertion p l) accum))
  C.LoopInvariant p b l -> do
    accum <- wpStmts' [LoopInvariant p b l] stmts post
    return (Accum [] (toStruct (LoopInvariant p b l) accum))
  otherStmt             -> do
    xs <- wpStmts' []                  stmts post
    x <- wp imposed otherStmt (precondAccum xs)
    return $ insert x xs

insert :: Stmt -> Accum -> Accum
insert x (Accum xs ys) = Accum (x:xs) ys

precondAccum :: Accum -> Pred
precondAccum (Accum []      xs) = precondStruct xs
precondAccum (Accum (stmt:_) _) = precond stmt

wp :: [Pred] -> C.Stmt -> Pred -> WPM Stmt
wp imposed stmt post = case stmt of
  C.Abort l              -> return $ Abort (L l (Constant C.false))
  C.Skip l               -> return $ Skip (L l post)
  C.Assert _ _           -> error "[ panic ] Assert in wp"
  C.LoopInvariant _ _ _  -> error "[ panic ] LoopInvariant in wp"
  C.Assign xs es l       -> do
    pre <- subst (assignmentEnv xs es) post
    return $ Assign (L l pre) xs es

  C.If gdCmds l -> do
    gdCmds' <- forM gdCmds $ \(C.GdCmd guard body m) -> do
      let imposed' = toGuard (IF l) guard : imposed
      struct <- wpStmts imposed' body post
      return $ GdCmd (Guard guard (IF l) m) struct

    let pre = disjunct (map (toGuard (IF l)) (C.getGuards gdCmds))
    return $ If (L l pre) gdCmds'

  C.Do gdCmds l -> case imposed of
    (LoopInvariant _ bnd _: _) -> do

      let loopInvariant' = conjunct $ reverse imposed

      -- use the loop invariant as the postcondition of the branch
      gdCmds' <- forM gdCmds $ \(C.GdCmd guard body m) -> do
        let imposed' = toGuard (LOOP l) guard : imposed
        struct <- wpStmts imposed' body loopInvariant'
        return $ GdCmd (Guard guard (LOOP l) m) struct

      return $ Do (L l loopInvariant') bnd gdCmds'

    _ -> throwError (MissingLoopInvariant l)

  C.SpecQM l            -> throwError (DigHole l)
  C.Spec l              -> return $ Spec (L l post)


programToStruct :: C.Program -> WPM Struct
programToStruct (C.Program _ stmts _) = case (init stmts, last stmts) of
  (C.Assert          p l:stmts', C.Assert q m) -> wpStmts [Assertion p l] stmts' (Assertion q m)
  (C.LoopInvariant p b l:stmts', C.Assert q m) -> wpStmts [LoopInvariant p b l] stmts' (Assertion q m)
  ([]                          , C.Assert _ l) -> throwError (MissingPrecondition l)
  (others               :_     , C.Assert _ _) -> throwError (MissingPrecondition (locOf others))
  (_                           , stmt)         -> throwError (MissingPostcondition (locOf stmt))
