module Syntax.Concrete where

import Data.Loc
import Data.Text.Lazy (Text)
import Prelude hiding (Ordering(..))

data Program = Program [Declaration] [Stmt] Loc
  deriving (Show)

data Declaration
  = ConstDecl [Upper] Type Loc
  | VarDecl [Lower] Type Loc
  deriving (Show)

data Stmt
  = Skip                      Loc
  | Abort                     Loc
  | Assign  [Lower] [Expr]    Loc
  | Assert  Expr              Loc
  | AssertWithBnd  Expr Expr  Loc
  | Do            [GdCmd]     Loc
  | If            [GdCmd]     Loc
  | SpecQM                    Loc -- ?      to be rewritten as {!!} by the frontend
  | Spec                      Loc
  deriving (Show)

data GdCmd = GdCmd Expr [Stmt] Loc deriving (Show)

--------------------------------------------------------------------------------
-- | Types

data Endpoint = Including Expr | Excluding Expr deriving (Show)
data Interval = Interval Endpoint Endpoint Loc deriving (Show)

data Type = TInt Loc | TBool Loc
          | TArray Interval Type Loc
          | TFunc Type Type Loc
          | TVar Int Loc
          deriving (Show)

instance Located Type where
  locOf (TInt loc) = loc
  locOf (TBool loc) = loc
  locOf (TArray _ _ loc) = loc
  locOf (TFunc _ _ loc) = loc
  locOf (TVar _ loc) = loc

--------------------------------------------------------------------------------
-- | Expressions

data Fixity = Infix Int | InfixR Int | InfixL Int | Prefix Int | Postfix Int
  deriving (Show, Eq)

data Op = EQ Loc | NEQ Loc | LTE Loc | GTE Loc | LT Loc | GT Loc
        | Implies Loc | Conj Loc | Disj Loc | Neg Loc
        | Add Loc | Sub Loc | Mul Loc | Div Loc
        deriving (Show)

data Lit  = Num Int
          | Bol Bool
          deriving Show

data Expr = Lit   Lit       Loc
          | Var   Lower     Loc
          | Const Upper     Loc
          | Op    Op        Loc
          | App   Expr Expr Loc
          | Hole            Loc
          deriving Show

instance Located Op where
  locOf (EQ l)      = l
  locOf (NEQ l)     = l
  locOf (LTE l)     = l
  locOf (GTE l)     = l
  locOf (LT l)      = l
  locOf (GT l)      = l
  locOf (Implies l) = l
  locOf (Conj l)    = l
  locOf (Disj l)    = l
  locOf (Neg l)     = l
  locOf (Add l)     = l
  locOf (Sub l)     = l
  locOf (Mul l)     = l
  locOf (Div l)     = l

instance Located Expr where
  locOf (Var _ l)   = l
  locOf (Const _ l)   = l
  locOf (Lit _ l)   = l
  locOf (Op _ l)    = l
  locOf (App _ _ l) = l
  locOf (Hole l)    = l


classify :: Op -> Fixity
classify (Implies _ ) = InfixR 1
classify (Disj _) = InfixL 2
classify (Conj _) = InfixL 3
classify (Neg _) = Prefix 4
classify (EQ _) = Infix 5
classify (NEQ _) = Infix 5
classify (LTE _) = Infix 5
classify (GTE _) = Infix 5
classify (LT _) = Infix 5
classify (GT _) = Infix 5
classify (Mul _) = InfixL 1
classify (Div _) = InfixL 1
classify (Add _) = InfixL 2
classify (Sub _) = InfixL 2

--------------------------------------------------------------------------------
-- | Variables and stuff

data Upper = Upper Text Loc
  deriving (Show)

data Lower = Lower Text Loc
  deriving (Show)
