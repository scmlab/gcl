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
                  Pred            -- precondition
                  Stmt            -- the statement
                  [Lasagna]       -- sub-lasagne for statements like IF, DO
                  Lasagna         -- following layers
             | Final Pred
             deriving (Show)

-- access the precondition of a Lasagna
precond :: Lasagna -> Pred
precond (Final p) = p
precond (Layer p _ _ _) = p


toLasagna :: [Stmt] -> Pred -> WPM Lasagna
toLasagna []     post = return $ Final post
toLasagna (x:xs) post = do
  xs' <- toLasagna xs post
  (pre, subs) <- wp x (precond xs')
  return $ Layer pre x subs xs'
  where
    wp :: Stmt -> Pred -> WPM (Pred, [Lasagna])
    wp (C.Abort _)              _    = return (Constant C.false   , [])
    wp (C.Skip _)               post = return (post               , [])
    wp (C.Assert p l)           _    = return (Assertion p l      , [])
    wp (C.LoopInvariant p _ l)  _    = return (LoopInvariant p l  , [])
    wp (C.Assign xs es _)       post = do
      pre <- subst (assignmentEnv xs es) post
      return (pre, [])
    wp (C.If gdcmds l)           _    = do
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

genObli :: [Pred]       -- additional Predicates to be conjuncted with
        -> Lasagna
        -> ObliM ()
genObli _ (Final _) = return ()
genObli pres (Layer preOfStmt stmt subs stmts) = do
  let post = precond stmts
  let pre = if null pres then preOfStmt else Conjunct (preOfStmt:pres)

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

  -- structStmts b (Conjunct [pre, toGuard (IF l) guard]) Nothing body post

      return ()


      -- forM_ gdcmds $ \(C.GdCmd guard body _) ->
      --
      --
      --   structStmts b (toGuard (IF l) guard) Nothing body post
      -- return (Disjunct (map (toGuard (IF l)) $ C.getGuards gdcmds)) -- is this enough?


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
