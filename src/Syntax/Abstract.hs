{-# LANGUAGE DeriveGeneric #-}

module Syntax.Abstract where

import Data.Aeson
import Data.Loc
import Data.Map (Map)
import Data.Text.Lazy (Text)
import GHC.Generics (Generic)
import Syntax.Common (Fixity (..))
import Prelude hiding (Ordering (..))
import Data.Function (on)

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

type Defns = Map Name Expr

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
  | SpecQM Loc -- ? to be rewritten as {!!} by the frontend
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
  locOf (SpecQM l) = l
  locOf (Spec _ l) = l
  locOf (Proof l) = l

data GdCmd = GdCmd Expr [Stmt] Loc deriving (Eq, Show)

extractAssertion :: Declaration -> Maybe Expr
extractAssertion (ConstDecl _ _ e _) = e
extractAssertion (VarDecl _ _ e _) = e
extractAssertion LetDecl {} = Nothing

extractLetBinding :: Declaration -> Maybe (Name, Expr)
extractLetBinding ConstDecl {} = Nothing
extractLetBinding VarDecl {} = Nothing
extractLetBinding (LetDecl name args expr _) = Just (name, wrapLam (map nameToText args) expr)

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
  | Op Op Loc
  | App Expr Expr Loc
  | Lam Text Expr Loc
  | Hole Loc
  | Quant Expr [Name] Expr Expr Loc
  | Subst Expr Subst -- internal. Location not necessary?
  deriving (Eq, Show, Generic)

type Subst = Map Text Expr

instance ToJSON Expr

instance FromJSON Expr

wrapLam :: [Text] -> Expr -> Expr
wrapLam [] body = body
wrapLam (x : xs) body = Lam x (wrapLam xs body) NoLoc

--------------------------------------------------------------------------------

-- | Variables and stuff
data Name = Name Text Loc
  deriving (Show, Generic)

-- | Compare regardless of their locations 
instance Eq Name where 
  (==) = (==) `on` nameToText

instance Located Name where
  locOf (Name _ l) = l

instance ToJSON Name

instance FromJSON Name

instance Ord Name where
  compare (Name a _) (Name b _) = compare a b

nameToText :: Name -> Text
nameToText (Name x _) = x

----------------------------------------------------------------

-- | Literals
data Lit = Num Int | Bol Bool | Chr Char
  deriving (Show, Eq, Generic)

instance Located Lit where
  locOf _ = NoLoc

instance FromJSON Lit

instance ToJSON Lit

----------------------------------------------------------------

-- | Operators
data Op
  = -- binary relations
    EQ
  | NEQ
  | LTE
  | GTE
  | LT
  | GT
  | -- logic operators
    Implies
  | Conj
  | Disj
  | -- arithmetics
    Neg
  | Add
  | Sub
  | Mul
  | Div
  | Mod
  | -- For Quant
    Sum
  | Forall
  | Exists
  deriving (Show, Eq, Generic)

instance ToJSON Op

instance FromJSON Op

classify :: Op -> Fixity
classify Implies = InfixR 1
classify Disj = InfixL 2
classify Conj = InfixL 3
classify Neg = Prefix 4
classify EQ = Infix 5
classify NEQ = Infix 6
classify LTE = Infix 6
classify GTE = Infix 6
classify LT = Infix 6
classify GT = Infix 6
classify Add = InfixL 7
classify Sub = InfixL 7
classify Mul = InfixL 8
classify Div = InfixL 8
classify Mod = InfixL 9
classify Sum = Prefix 5
classify Exists = Prefix 6
classify Forall = Prefix 7

-- | Constructors
unary :: Op -> Expr -> Expr
unary op x = App (Op op NoLoc) x NoLoc

binary :: Op -> Expr -> Expr -> Expr
binary op x y = App (App (Op op NoLoc) x NoLoc) y (x <--> y)

lt, gt, gte, lte, eqq, conj, disj, implies :: Expr -> Expr -> Expr
lt = binary LT
gt = binary GT
gte = binary GTE
lte = binary LTE
eqq = binary EQ
conj = binary Conj
disj = binary Disj
implies = binary Implies

neg :: Expr -> Expr
neg = unary Neg

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
imply p q = App (App (Op Implies NoLoc) p NoLoc) q NoLoc

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
  locOf (Op _ l) = l
  locOf (App _ _ l) = l
  locOf (Lam _ _ l) = l
  locOf (Hole l) = l
  locOf (Quant _ _ _ _ l) = l
  locOf (Subst _ _) = NoLoc
