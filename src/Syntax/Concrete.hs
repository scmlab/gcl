{-# LANGUAGE OverloadedStrings #-}
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
      [(Upper, [Text], Expr)]  -- let bindings
      [Stmt]                   -- main program
      Loc
  deriving (Eq, Show)

data Declaration
  = ConstDecl [Upper] Type (Maybe Expr) Loc
  | VarDecl [Lower] Type (Maybe Expr) Loc
  | LetDecl Upper [Text] Expr Loc
  deriving (Eq, Show)

data Stmt
  = Skip                      Loc
  | Abort                     Loc
  | Assign  [Lower] [Expr]    Loc
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
extractAssertion (LetDecl _ _ _ _  ) = Nothing

extractLetBinding :: Declaration -> Maybe (Upper, [Text], Expr)
extractLetBinding (ConstDecl _ _ _ _) = Nothing
extractLetBinding (VarDecl   _ _ _ _) = Nothing
extractLetBinding (LetDecl c a e _  ) = Just (c, a, e)

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
          | TVar Lower Loc
          deriving (Eq, Show)

--------------------------------------------------------------------------------
-- | Expressions

data Expr = Lit   Lit       Loc
          | Var   Lower     Loc
          | Const Upper     Loc
          | Op    Op        Loc
          | App   Expr Expr Loc
          | Lam   Text Expr Loc
          | Hole            Loc
          | Quant Expr [Lower] Expr Expr Loc
          deriving (Eq, Show, Generic)

instance ToJSON Expr where

--------------------------------------------------------------------------------
-- | Variables and stuff

data Upper = Upper Text Loc
  deriving (Eq, Show, Generic)

instance ToJSON Upper where
instance Ord Upper where
  compare (Upper a _) (Upper b _) = compare a b

data Lower = Lower Text Loc
  deriving (Eq, Show, Generic)

instance ToJSON Lower where
instance Ord Lower where
  compare (Lower a _) (Lower b _) = compare a b

upperToText :: Upper -> Text
upperToText (Upper x _) = x

lowerToText :: Lower -> Text
lowerToText (Lower x _) = x

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
constant x = Const (Upper x NoLoc) NoLoc

variable :: Text -> Expr
variable x = Var (Lower x NoLoc) NoLoc

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
