module Syntax.Concrete where

import Data.Loc
import Data.Text (Text)

data Program = Program [Declaration] [Statement] Loc
  deriving (Show)

data Declaration
  = ConstDecl [Constant] Type Loc
  | VarDecl [Variable] Type Loc
  deriving (Show)

data Statement
  = Skip Loc
  | Abort Loc
  | Assign [Variable] [Expr] Loc
  | Assert Pred Loc
  | Do Expr [Branch] Loc
  deriving (Show)

data Branch = Branch Pred [Statement] Loc deriving (Show)

--------------------------------------------------------------------------------
-- | Predicates

data BinRel = Eq Loc | LEq Loc | GEq Loc | LTh Loc | GTh Loc
  deriving Show

data Pred = Term    Expr BinRel Expr  Loc
          | Implies Pred Pred         Loc
          | Conj    Pred Pred         Loc
          | Disj    Pred Pred         Loc
          | Neg     Pred              Loc
          | Hole                      Loc
          deriving (Show)

instance Located Pred where
  locOf (Term _ _ _ l)  = l
  locOf (Implies _ _ l) = l
  locOf (Conj _ _ l)    = l
  locOf (Disj _ _ l)    = l
  locOf (Neg _ l)       = l
  locOf (Hole l)        = l

--------------------------------------------------------------------------------
-- | Expressions

data Lit  = Num Int
          | Bol Bool
          deriving Show

type OpName = Text
data Expr = Var Variable              Loc
          | Const Constant            Loc
          | Lit Lit                   Loc
          | Op  OpName [Expr]         Loc
          | HoleE                     Loc
          deriving Show

--------------------------------------------------------------------------------
-- | Variables and stuff

data Constant = Constant Text Loc
  deriving (Show)

data Variable = Variable Text Loc
  deriving (Show)

data Type = Type Text Loc
  deriving (Show)
