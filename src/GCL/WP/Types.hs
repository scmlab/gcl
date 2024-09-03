{-# LANGUAGE DeriveGeneric, FlexibleContexts,
             FlexibleInstances #-}

module GCL.WP.Types where

import           GHC.Generics                   ( Generic )
import           Control.Monad.Except           ( Except )
import           Control.Monad.RWS              ( MonadState(..)
                                                , RWST(..) )
import Data.IntMap                              ( IntMap )
import Data.Aeson (ToJSON)
import Data.Text  ( Text )
import Data.Map                                 ( Map )
import Data.Loc (Loc (..), Located (..))
import Data.Loc.Range (Range)
import GCL.Common
import GCL.Predicate                           ( InfMode(..)
                                               , PO(..), Pred, Spec (..))
import           Syntax.Typed

-- The WP monad.

type TM = Except StructError

type Decls = Map Text (Maybe Expr)

type WP
  = RWST
      (Decls, [[Text]])
      ([PO], [Spec], [StructWarning], IntMap (Int, Expr))
      Int
      TM

instance Counterous WP where
  countUp = do i <- get
               put (succ i)
               return i

---

data SegElm = SAsrt Stmt
            | SSpec Stmt
            | SStmts [Stmt]

-- types of mutually recursive functions

type TstructStmts = InfMode -> (Pred, Maybe Expr) -> [Stmt] -> Pred -> WP ()
type TstructSegs  = (Pred, Maybe Expr) -> [SegElm] -> Pred -> WP ()
type Tstruct      = (Pred, Maybe Expr) -> Stmt -> Pred -> WP ()

type TwpSegs   = [SegElm] -> Pred -> WP Pred
type TwpSStmts = [Stmt]   -> Pred -> WP Pred
type Twp       = Stmt     -> Pred -> WP Pred

type TspStmts  = (Pred, Maybe Expr) -> [Stmt] -> WP Pred
type TspSegs   = (Pred, Maybe Expr) -> [SegElm] -> WP Pred
type TspSStmts = (Pred, Maybe Expr) -> [Stmt] -> WP Pred

---

data StructWarning
  = MissingBound Range
  deriving (Eq, Show, Generic)

instance Located StructWarning where
  locOf (MissingBound rng) = locOf rng

data StructError
  = MissingAssertion Loc
  | MissingPostcondition Loc
  | MultiDimArrayAsgnNotImp Loc
     -- Assignment to multi-dimensional array not implemented.
     -- SCM: will remove this when we figure out how.
  | LocalVarExceedScope Loc
  deriving (Eq, Show, Generic)

instance Located StructError where
  locOf (MissingAssertion l) = l
  locOf (MissingPostcondition l) = l
  locOf (MultiDimArrayAsgnNotImp l) = l
  locOf (LocalVarExceedScope l) = l

-- freshPreInScope prefix scope
--   generates a fresh name, with prefix, that does not appear in scope
