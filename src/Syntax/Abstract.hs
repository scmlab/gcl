{-# LANGUAGE DeriveGeneric #-}

module Syntax.Abstract where

import Data.Aeson
import Data.Loc
import Data.Map (Map)
import Data.Text (Text)
import GHC.Generics (Generic)
import Syntax.Common
import Prelude hiding (Ordering (..))

--------------------------------------------------------------------------------

type Const = Text

type Var = Text

type TypeVar = Text

--------------------------------------------------------------------------------

-- | Program / Declaration / Statement
data Program
  = Program
      [Declaration] -- constant and variable declarations
      [Expr] -- global properties
      Defns -- let bindings
      [Stmt] -- main program
      Loc
  deriving (Eq, Show)

data Defn = Defn
  { defnName :: Name,
    defnExpr :: Expr
  }
  deriving (Eq, Show)

type Defns = Map Text Defn

data Declaration
  = ConstDecl [Name] Type (Maybe Expr) Loc
  | VarDecl [Name] Type (Maybe Expr) Loc
  | LetDecl Name [Name] Expr Loc
  deriving (Eq, Show)

data Stmt
  = Skip Loc
  | Abort Loc
  | Assign [Name] [Expr] Loc
  | Assert Expr Loc
  | LoopInvariant Expr Expr Loc
  | Do [GdCmd] Loc
  | If [GdCmd] Loc
  | Spec Text Loc
  | Proof Loc
  deriving (Eq, Show)

data GdCmd = GdCmd Expr [Stmt] Loc deriving (Eq, Show)

extractAssertion :: Declaration -> Maybe Expr
extractAssertion (ConstDecl _ _ e _) = e
extractAssertion (VarDecl _ _ e _) = e
extractAssertion LetDecl {} = Nothing

extractLetBinding :: Declaration -> Maybe (Text, Defn)
extractLetBinding ConstDecl {} = Nothing
extractLetBinding VarDecl {} = Nothing
extractLetBinding (LetDecl name args expr _) = Just (nameToText name, Defn name (wrapLam args expr))

getGuards :: [GdCmd] -> [Expr]
getGuards = fst . unzipGdCmds

unzipGdCmds :: [GdCmd] -> ([Expr], [[Stmt]])
unzipGdCmds = unzip . map (\(GdCmd x y _) -> (x, y))

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
  = Lit Lit Loc
  | Var Name Loc
  | Const Name Loc
  | Op ArithOp
  | Chain Expr ChainOp Expr Loc
  | App Expr Expr Loc
  | Lam Name Expr Loc
  | Hole Loc
  | Quant QuantOp' [Name] Expr Expr Loc
  | Subst Expr Subst Expr
  deriving (Eq, Show, Generic)

type QuantOp' = Either Op Expr

type Subst = Map Name Expr

instance ToJSON Expr

instance FromJSON Expr

wrapLam :: [Name] -> Expr -> Expr
wrapLam [] body = body
wrapLam (x : xs) body = Lam x (wrapLam xs body) NoLoc

----------------------------------------------------------------

-- | Literals
data Lit = Num Int | Bol Bool | Chr Char
  deriving (Show, Eq, Generic)


instance FromJSON Lit

instance ToJSON Lit

----------------------------------------------------------------
