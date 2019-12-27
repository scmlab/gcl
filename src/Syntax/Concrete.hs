module Syntax.Concrete where

import Data.Loc
import Data.Text.Lazy (Text)

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
-- | Predicates

-- data BinRel = EQ Loc | LTE Loc | GTE Loc | LT Loc | GT Loc
--   deriving Show

-- data Pred = Term    Expr BinRel Expr  Loc
--           | Implies Pred Pred         Loc
--           | Conj    Pred Pred         Loc
--           | Disj    Pred Pred         Loc
--           | Neg     Pred              Loc
--           | Lit     Bool              Loc
--           | HoleP                     Loc
          -- deriving (Show)

-- instance Located Pred where
--   locOf (Term _ _ _ l)  = l
--   locOf (Implies _ _ l) = l
--   locOf (Conj _ _ l)    = l
--   locOf (Disj _ _ l)    = l
--   locOf (Neg _ l)       = l
--   locOf (Lit _ l)       = l
--   locOf (HoleP l)       = l

--------------------------------------------------------------------------------
-- | Expressions

data Op = EQ Loc | LTE Loc | GTE Loc | LT Loc | GT Loc
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

instance Located Expr where
  locOf (Lit _ l)   = l
  locOf (Op _ l)    = l
  locOf (App _ _ l) = l
  locOf (Hole l)    = l

--------------------------------------------------------------------------------
-- | Variables and stuff

data Type = Type Text Loc
  deriving (Show)

data Upper = Upper Text Loc
  deriving (Show)

data Lower = Lower Text Loc
  deriving (Show)
