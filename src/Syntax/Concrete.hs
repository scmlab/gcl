module Syntax.Concrete where

import Data.Loc
import Data.Text.Lazy (Text)

data Program = Program [Declaration] [Stmt] Loc
  deriving (Show)

data Declaration
  = ConstDecl [Const] Type Loc
  | VarDecl [Var] Type Loc
  deriving (Show)

data Stmt
  = Skip                      Loc
  | Abort                     Loc
  | Assign  [Var] [Expr]      Loc
  | Assert  Pred              Loc
  | AssertWithBnd  Pred Expr  Loc
  | Do            [GdCmd]     Loc
  | If            [GdCmd]     Loc
  | Hole                      Loc -- ?      to be rewritten as {!!} by the frontend
  | Spec                      Loc
  deriving (Show)

data GdCmd = GdCmd Pred [Stmt] Loc deriving (Show)

--------------------------------------------------------------------------------
-- | Predicates

data BinRel = Eq Loc | LEq Loc | GEq Loc | LTh Loc | GTh Loc
  deriving Show

data Pred = Term    Expr BinRel Expr  Loc
          | Implies Pred Pred         Loc
          | Conj    Pred Pred         Loc
          | Disj    Pred Pred         Loc
          | Neg     Pred              Loc
          | Lit     Bool              Loc
          | HoleP                     Loc
          deriving (Show)

instance Located Pred where
  locOf (Term _ _ _ l)  = l
  locOf (Implies _ _ l) = l
  locOf (Conj _ _ l)    = l
  locOf (Disj _ _ l)    = l
  locOf (Neg _ l)       = l
  locOf (Lit _ l)       = l
  locOf (HoleP l)       = l

--------------------------------------------------------------------------------
-- | Expressions

data Lit  = Num Int
          | Bol Bool
          deriving Show

data Expr = VarE    Var           Loc
          | ConstE  Const         Loc
          | LitE    Lit           Loc
          | OpE     Expr   [Expr] Loc
          | HoleE                 Loc
          deriving Show

--------------------------------------------------------------------------------
-- | Variables and stuff

data Const = Const Text Loc
  deriving (Show)

data Var = Var Text Loc
  deriving (Show)

data Type = Type Text Loc
  deriving (Show)
