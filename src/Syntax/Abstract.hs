{-# LANGUAGE DeriveGeneric #-}

module Syntax.Abstract where

import Data.Aeson
import Data.Loc
import Data.Map (Map)
import Data.Text (Text)
import GHC.Generics (Generic)
import Syntax.Common
import Prelude hiding (Ordering (..))
import Data.Loc.Range

--------------------------------------------------------------------------------

type Const = Text

type Var = Text

type TypeVar = Text

--------------------------------------------------------------------------------

-- | Program
data Program
  = Program
      [Declaration] -- constant and variable declarations
      [Expr] -- global properties
      Defns -- let bindings
      [Stmt] -- main program
      Loc
  deriving (Eq, Show)

type Defns = Map Name Expr

--------------------------------------------------------------------------------

-- | Declaration
data Declaration
  = ConstDecl [Name] Type (Maybe Expr) Loc
  | VarDecl [Name] Type (Maybe Expr) Loc
  | LetDecl Name [Name] Expr Loc
  deriving (Eq, Show)

instance Located Declaration where
  locOf (ConstDecl _ _ _ l) = l
  locOf (VarDecl _ _ _ l) = l
  locOf (LetDecl _ _ _ l) = l

--------------------------------------------------------------------------------

data Stmt
  = Skip Loc
  | Abort Loc
  | Assign [Name] [Expr] Loc
  | AAssign Name Expr Expr Loc
  | Assert Expr Loc
  | LoopInvariant Expr Expr Loc
  | Do [GdCmd] Loc
  | If [GdCmd] Loc
  | Spec Text Range
  | Proof Loc
  deriving (Eq, Show)

data GdCmd = GdCmd Expr [Stmt] Loc deriving (Eq, Show)

instance Located GdCmd where
  locOf (GdCmd _ _ l) = l

--------------------------------------------------------------------------------

-- | Endpoint
data Endpoint = Including Expr | Excluding Expr deriving (Eq, Show, Generic)

instance ToJSON Endpoint


-- | Interval
data Interval = Interval Endpoint Endpoint Loc deriving (Eq, Show, Generic)

instance ToJSON Interval

-- | Base Types
data TBase = TInt | TBool | TChar
  deriving (Show, Eq, Generic)

instance ToJSON TBase

-- | Types
data Type
  = TBase TBase Loc
  | TArray Interval Type Loc
  | TFunc Type Type Loc
  | TVar Name Loc
  deriving (Eq, Show, Generic)

instance ToJSON Type

--------------------------------------------------------------------------------

-- | Expressions
data Expr
  = Paren Expr
  | Lit Lit Loc
  | Var Name Loc
  | Const Name Loc
  | Op Op
  | Chain Expr Op Expr Loc
  | App Expr Expr Loc
  | Lam Name Expr Loc
  | Hole Loc
  | Quant Expr [Name] Expr Expr Loc
  | Subst Expr Subst Expr
  | ArrUpd Expr Expr Expr Loc
  deriving (Eq, Show, Generic)

type QuantOp' = Either Op Expr

type Subst = Map Name Expr

instance ToJSON Expr

instance FromJSON Expr

----------------------------------------------------------------

-- | Literals
data Lit = Num Int | Bol Bool | Chr Char
  deriving (Show, Eq, Generic)


instance FromJSON Lit

instance ToJSON Lit

----------------------------------------------------------------
