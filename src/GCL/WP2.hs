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
import Syntax.Location ()


--------------------------------------------------------------------------------
-- | Lasagna, alternating sequence of Pred & Statement

data Lasagna = Layer
                  (Maybe Stmt)    -- the previous statement
                  Pred            -- precondition
                  Stmt            -- the current statement
                  [Lasagna]       -- sub-lasagne of statements like IF, DO
                  Lasagna         -- following layers
             | Final Pred
             deriving (Show)

-- access the precondition of a Lasagna
precond :: Lasagna -> Pred
precond (Final p) = p
precond (Layer _ p _ _ _) = p


toLasagna :: [Stmt] -> Pred -> WPM Lasagna
toLasagna = toLasagna' Nothing
  where

    toLasagna' :: Maybe Stmt -> [Stmt] -> Pred -> WPM Lasagna
    toLasagna' _        []     post = return $ Final post
    toLasagna' previous (x:xs) post = do
      xs' <- toLasagna' (Just x) xs post
      (pre, subs) <- wp x (precond xs')
      return $ Layer previous pre x subs xs'

    wp :: Stmt -> Pred -> WPM (Pred, [Lasagna])
    wp (C.Abort _)              _    = return (Constant C.false   , [])
    wp (C.Skip _)               post = return (post               , [])
    wp (C.Assert p l)           _    = return (Assertion p l      , [])
    wp (C.LoopInvariant p _ l)  _    = return (LoopInvariant p l  , [])
    wp (C.Assign xs es _)       post = do
      pre <- subst (assignmentEnv xs es) post
      return (pre, [])
    wp (C.If gdcmds l)          post = do
      subs <- mapM (gdCmdToLasagna post) gdcmds
      return (Disjunct (map (toGuard (IF l)) $ C.getGuards gdcmds), subs)
    wp (C.Do _ l)               _    = throwError (MissingAssertion l)
    wp (C.SpecQM l)             _    = throwError (DigHole l)
    wp (C.Spec _)               post = return (post, [])


    gdCmdToLasagna :: Pred -> C.GdCmd -> WPM Lasagna
    gdCmdToLasagna post (C.GdCmd _ body _) = toLasagna body post

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

-- Monad on top of WPM, for generating obligations
type ObliM = WriterT [Obligation2] (StateT Int WPM)

instance C.Fresh ObliM where
  fresh = do
    i <- get
    put (succ i)
    return i

runObliM :: ObliM a -> Either StructError2 (a, [Obligation2])
runObliM f = runWPM (evalStateT (runWriterT f) 0)

tellObli :: Pred -> Pred -> ObliOrigin2 -> ObliM ()
tellObli p q l = do
  -- NOTE: this could use some love
  unless (C.predEq (toExpr p) (toExpr q)) $ do
    i <- get
    put (succ i)
    tell [Obligation i p q l]

  -- -> [Pred]       -- additional postconditions to be disjuncted with


-- genObli :: [Pred]
--         -> [Pred]

genObli :: [Pred]       -- additional preconditions to be conjuncted with
        -> Lasagna
        -> ObliM ()
genObli _ (Final _) = return ()
genObli pres (Layer previousStmt stmtPreCond stmt subs stmts) = do
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
    C.If gdcmds l -> do
      tellObli pre (Disjunct $ map (toGuard (IF l)) $ C.getGuards gdcmds) (IfTotal l)

      let zipped = zip gdcmds subs
      forM_ zipped $ \(C.GdCmd guard _ _, body) ->
        genObli [pre, toGuard (IF l) guard] body
    C.Do gdcmds l -> case previousStmt of
      (Just (C.LoopInvariant _ bnd _)) -> do
        -- let guards = C.getGuards gdcmds
        -- -- base case
        -- tellObli
        --   (map (Negate . toGuard (LOOP l)) guards ++ pre)
        --   post
        --   (LoopBase l)
        -- -- inductive cases
        -- let zipped = zip gdcmds subs
        -- forM_ zipped $ \(C.GdCmd guard _ _, body) ->
        --   genObli [pre, toGuard (LOOP l) guard] body
        -- -- termination
        -- tellObli
        --   (map (toGuard (LOOP l)) guards ++ pre)
        --   (Bound $ bnd `C.gte` (C.Lit (C.Num 0) NoLoc))
        --   (LoopTermBase l)

        -- -- bound decrementation
        -- oldbnd <- C.freshVar "bnd"
        -- forM_ gdcmds $ \(C.GdCmd guard body _) ->
        --   let pre' = pre ++
        --               [ Bound $ bnd `C.eqq` C.Var oldbnd NoLoc
        --               , toGuard (LOOP l) guard
        --               ]
        --   genObli pre' body
        --     Nothing
        --     body
        --     (Bound $ bnd `C.lt` C.Var oldbnd NoLoc)

        return ()


      _ -> throwError (MissingBound l)
       {- Or if we want to tolerate the user and carry on ---
       do -- warn that bnd is missing
        let gdcmds' = map (\(GdCmd x y _) -> (depart x, y)) gdcmds
        let guards = map fst gdcmds'
        obligate (inv `A.conj` (A.conjunct (map A.neg guards))) post
        --
        forM_ gdcmds' $ \(guard, body) -> do
          structStmts b (inv `A.conj` guard) Nothing body inv
       -}

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
-- | Specification

data Specification2 = Specification
  { specID       :: Int
  , specPreCond  :: Pred
  , specPostCond :: Pred
  , specLoc      :: Loc
  } deriving (Eq, Show, Generic)
