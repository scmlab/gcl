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

instance Located Program where
  locOf (Program _ _ _ _ l) = l

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

instance Located Stmt where
  locOf (Skip l) = l
  locOf (Abort l) = l
  locOf (Assign _ _ l) = l
  locOf (Assert _ l) = l
  locOf (LoopInvariant _ _ l) = l
  locOf (Do _ l) = l
  locOf (If _ l) = l
  locOf (Spec _ l) = l
  locOf (Proof l) = l

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

instance Located Endpoint where
  locOf (Including e) = locOf e
  locOf (Excluding e) = locOf e

-- | Interval
data Interval = Interval Endpoint Endpoint Loc deriving (Eq, Show, Generic)

instance ToJSON Interval

instance Located Interval where
  locOf (Interval _ _ l) = l

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

instance Located Type where
  locOf (TBase _ l) = l
  locOf (TArray _ _ l) = l
  locOf (TFunc _ _ l) = l
  locOf (TVar _ l) = l

--------------------------------------------------------------------------------

-- | Expressions
data Expr
  = Lit Lit Loc
  | Var Name Loc
  | Const Name Loc
  | Op Op
  | Chain Expr Op Expr Loc
  | App Expr Expr Loc
  | Lam Name Expr Loc
  | Hole Loc
  | Quant Expr [Name] Expr Expr Loc
  | Subst Expr Subst -- internal. Location not necessary?
  deriving (Eq, Show, Generic)

type Subst = Map Name Expr

instance ToJSON Expr

instance FromJSON Expr

wrapLam :: [Name] -> Expr -> Expr
wrapLam [] body = body
wrapLam (x : xs) body = Lam x (wrapLam xs body) NoLoc

chainRightMost :: Expr -> Expr
chainRightMost (Chain _ _ r _) = r
chainRightMost x = x

----------------------------------------------------------------

-- | Literals
data Lit = Num Int | Bol Bool | Chr Char
  deriving (Show, Eq, Generic)

instance Located Lit where
  locOf _ = NoLoc

instance FromJSON Lit

instance ToJSON Lit

----------------------------------------------------------------

-- | Constructors
unary :: Op -> Expr -> Expr
unary op x = App (Op op) x (x <--> op)

binary :: Op -> Expr -> Expr -> Expr
binary op x y = App (App (Op op) x (x <--> op)) y (x <--> y)

lt, gt, gte, lte, eqq, conj, disj, implies :: Expr -> Expr -> Expr
lt = binary (LT NoLoc)
gt = binary (GT NoLoc)
gte = binary (GTE NoLoc)
lte = binary (LTE NoLoc)
eqq = binary (EQ NoLoc)
conj = binary (Conj NoLoc)
disj = binary (Disj NoLoc)
implies = binary (Implies NoLoc)

neg :: Expr -> Expr
neg = unary (Neg NoLoc)

true :: Expr
true = Lit (Bol True) NoLoc

false :: Expr
false = Lit (Bol False) NoLoc

conjunct :: [Expr] -> Expr
conjunct [] = true
conjunct xs = foldl1 conj xs

disjunct :: [Expr] -> Expr
disjunct [] = false
disjunct xs = foldl1 disj xs

imply :: Expr -> Expr -> Expr
imply p q = App (App (Op (Implies NoLoc)) p (locOf p)) q (locOf q)

predEq :: Expr -> Expr -> Bool
predEq = (==)

constant :: Text -> Expr
constant x = Const (Name x NoLoc) NoLoc

variable :: Text -> Expr
variable x = Var (Name x NoLoc) NoLoc

number :: Int -> Expr
number n = Lit (Num n) NoLoc

--------------------------------------------------------------------------------

-- | Instance of Located
instance Located Expr where
  locOf (Var _ l) = l
  locOf (Const _ l) = l
  locOf (Lit _ l) = l
  locOf (Op op) = locOf op
  locOf (Chain _ _ _ l) = l
  locOf (App _ _ l) = l
  locOf (Lam _ _ l) = l
  locOf (Hole l) = l
  locOf (Quant _ _ _ _ l) = l
  locOf (Subst _ _) = NoLoc
