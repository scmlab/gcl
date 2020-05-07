{-# LANGUAGE DeriveGeneric #-}

module Syntax.Concrete
  ( module Syntax.Concrete
  , Op(..)
  , TBase(..)
  , Lit(..)  -- re-exporting from Syntax.Abstract
  )
where

import           Data.Aeson
import           Data.Loc
import           Data.Map                       ( Map )
import           Data.Text.Lazy                 ( Text )
import           Prelude                 hiding ( Ordering(..) )
import           GHC.Generics                   ( Generic )

import           Syntax.Abstract                ( Op(..)
                                                , TBase(..)
                                                , Lit(..)
                                                )

--------------------------------------------------------------------------------
-- | Program / Declaration / Statement

data Program = Program
      [Declaration]            -- constant and variable declarations
      [Expr]                   -- global properties
      Defns                    -- let bindings
      [Stmt]                   -- main program
      Loc
  deriving (Eq, Show)

type Defns = [(Text, Expr)]

data Declaration
  = ConstDecl [Name] Type (Maybe Expr) Loc
  | VarDecl [Name] Type (Maybe Expr) Loc
  | LetDecl Name [Text] Expr Loc
  deriving (Eq, Show)

data Stmt
  = Skip                      Loc
  | Abort                     Loc
  | Assign  [Name] [Expr]     Loc
  | Assert  Expr              Loc
  | LoopInvariant  Expr Expr  Loc
  | Do            [GdCmd]     Loc
  | If            [GdCmd]     Loc
  | SpecQM                    Loc -- ? to be rewritten as {!!} by the frontend
  | Spec                      Loc
  deriving (Eq, Show)

data GdCmd = GdCmd Expr [Stmt] Loc deriving (Eq, Show)

extractAssertion :: Declaration -> Maybe Expr
extractAssertion (ConstDecl _ _ e _) = e
extractAssertion (VarDecl   _ _ e _) = e
extractAssertion (LetDecl   _ _ _ _) = Nothing

extractLetBinding :: Declaration -> Maybe (Text, Expr)
extractLetBinding (ConstDecl _ _ _ _) = Nothing
extractLetBinding (VarDecl   _ _ _ _) = Nothing
extractLetBinding (LetDecl   c a e _) = Just (nameToText c, wrapLam a e)

getGuards :: [GdCmd] -> [Expr]
getGuards = fst . unzipGdCmds

unzipGdCmds :: [GdCmd] -> ([Expr], [[Stmt]])
unzipGdCmds = unzip . map (\(GdCmd x y _) -> (x, y))

--------------------------------------------------------------------------------
-- | Types

data Endpoint = Including Expr | Excluding Expr deriving (Eq, Show)
data Interval = Interval Endpoint Endpoint Loc deriving (Eq, Show)

data Type = TBase TBase Loc
          | TArray Interval Type Loc
          | TFunc Type Type Loc
          | TVar Name Loc
          deriving (Eq, Show)

--------------------------------------------------------------------------------
-- | Expressions

data Expr = Lit   Lit       Loc
          | Var   Name      Loc
          | Const Name      Loc
          | Op    Op        Loc
          | App   Expr Expr Loc
          | Lam   Text Expr Loc
          | Hole            Loc
          | Quant Expr [Name] Expr Expr Loc
          | Subst Expr Subst -- internal. Location not necessary?
          deriving (Eq, Show, Generic)

type Subst = Map Text Expr

instance ToJSON Expr where
instance FromJSON Expr where

wrapLam :: [Text] -> Expr -> Expr
wrapLam []       body = body
wrapLam (x : xs) body = Lam x (wrapLam xs body) NoLoc

--------------------------------------------------------------------------------
-- | Variables and stuff

data Name = Name Text Loc
  deriving (Eq, Show, Generic)

instance ToJSON Name where
instance FromJSON Name where

instance Ord Name where
  compare (Name a _) (Name b _) = compare a b

nameToText :: Name -> Text
nameToText (Name x _) = x

--------------------------------------------------------------------------------
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
  locOf (Var   _ l      ) = l
  locOf (Const _ l      ) = l
  locOf (Lit   _ l      ) = l
  locOf (Op    _ l      ) = l
  locOf (App _ _ l      ) = l
  locOf (Lam _ _ l      ) = l
  locOf (Hole l         ) = l
  locOf (Quant _ _ _ _ l) = l
  locOf (Subst _ _      ) = NoLoc
