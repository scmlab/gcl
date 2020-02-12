{-# LANGUAGE OverloadedStrings, FlexibleContexts, DeriveGeneric,
             TypeSynonymInstances, FlexibleInstances #-}

module GCL.WP2 where

import Control.Monad.State hiding (guard)
-- import Control.Monad.Writer hiding (guard)
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


runWPM :: WPM a -> Either StructError2 a
runWPM f = evalState (runExceptT f) 0

type WPM = ExceptT StructError2 (State Int)

instance C.Fresh WPM where
  fresh = do
    i <- get
    put (succ i)
    return i

--------------------------------------------------------------------------------
-- | Lasagna, alternating sequence of Pred & Statement

wp :: Stmt -> Pred -> WPM Pred
wp (C.Abort _)              _    = return (Constant C.false)
wp (C.Skip _)               post = return post
wp (C.Assert p l)           _    = return (Assertion p l)
wp (C.LoopInvariant p _ l)  _    = return (LoopInvariant p l)
wp (C.Assign xs es _)       post = subst (assignmentEnv xs es) post
wp (C.If gcmds l)           _    = return (Disjunct (map (toGuard (IF l)) $ C.getGuards gcmds))
wp (C.Do _ l)               _    = throwError (MissingAssertion l)
wp (C.SpecQM l)             _    = throwError $ DigHole l
wp (C.Spec _)               post = return post

assignmentEnv :: [Lower] -> [Expr] -> C.Subst
assignmentEnv xs es = Map.fromList (zip (map Left xs) es)

data Lasagna = Layer
                  Pred      -- precondition
                  Stmt      -- the statement
                  Lasagna   -- following layers
             | Final Pred
             deriving (Show)

precond :: Lasagna -> Pred
precond (Final p) = p
precond (Layer p _ _) = p

toLasagna :: C.Program -> WPM Lasagna
toLasagna (C.Program _ stmts _) = case (init stmts, last stmts) of
  (stmts', C.Assert p l) -> toLasagna' stmts' (Assertion p l)
  (_     , stmt)         -> throwError (MissingPostcondition (locOf stmt))
  where
    toLasagna' :: [Stmt] -> Pred -> WPM Lasagna
    toLasagna' []     post = return $ Final post
    toLasagna' (x:xs) post = do
      xs' <- toLasagna' xs post
      let post' = precond xs'
      pre <- wp x post'
      return $ Layer pre x xs'

wpProgram :: C.Program -> WPM Pred
wpProgram p = precond <$> toLasagna p

-- program :: C.Program -> WPM Lasagna
-- wpProgram (C.Program _ stmts _) = case (init stmts, last stmts) of
--     (stmts', C.Assert p l) -> toLasagna stmts' (Assertion p l)
--     (_     , stmt)         -> throwError (MissingPostcondition (locOf stmt))



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
-- | Obligations

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

--------------------------------------------------------------------------------
-- | Specification

data Specification2 = Specification
  { specID       :: Int
  , specPreCond  :: Pred
  , specPostCond :: Pred
  , specLoc      :: Loc
  } deriving (Eq, Show, Generic)
