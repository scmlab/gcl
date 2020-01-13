module Syntax.Concrete where

import Data.Loc
import Data.Text.Lazy (Text)
import Prelude hiding (Ordering(..))

data Program = Program [Declaration] [Stmt] Loc
  deriving (Eq, Show)

data Declaration
  = ConstDecl [Upper] Type Loc
  | VarDecl [Lower] Type Loc
  deriving (Eq, Show)

data Stmt
  = Skip                      Loc
  | Abort                     Loc
  | Assign  [Lower] [Expr]    Loc
  | Assert  Expr              Loc
  | AssertWithBnd  Expr Expr  Loc
  | Do            [GdCmd]     Loc
  | If            [GdCmd]     Loc
  | SpecQM                    Loc -- ? to be rewritten as {!!} by the frontend
  | Spec                      Loc
  deriving (Eq, Show)

data GdCmd = GdCmd Expr [Stmt] Loc deriving (Eq, Show)

getGuards :: [GdCmd] -> [Expr]
getGuards = fst . unzipGdCmds

unzipGdCmds :: [GdCmd] -> ([Expr], [[Stmt]])
unzipGdCmds = unzip . map (\(GdCmd x y _) -> (x, y))

--------------------------------------------------------------------------------
-- | Types

data Endpoint = Including Expr | Excluding Expr deriving (Eq, Show)
data Interval = Interval Endpoint Endpoint Loc deriving (Eq, Show)

data Base = TInt | TBool | TChar
      deriving (Eq, Show)

data Type = TBase Base Loc
          | TArray Interval Type Loc
          | TFunc Type Type Loc
          | TVar Lower Loc
          deriving (Eq, Show)

--------------------------------------------------------------------------------
-- | Expressions

data Fixity = Infix Int | InfixR Int | InfixL Int | Prefix Int | Postfix Int
  deriving (Show, Eq)

data Op = EQ Loc | NEQ Loc | LTE Loc | GTE Loc | LT Loc | GT Loc
        | Implies Loc | Conj Loc | Disj Loc | Neg Loc
        | Add Loc | Sub Loc | Mul Loc | Div Loc | Mod Loc
        deriving (Eq, Show)

data Lit  = Num Int
          | Bol Bool
          deriving (Eq, Show)

data Expr = Lit   Lit       Loc
          | Var   Lower     Loc
          | Const Upper     Loc
          | Op    Op        Loc
          | App   Expr Expr Loc
          | Hole            Loc
          deriving (Eq, Show)

--------------------------------------------------------------------------------
-- | Variables and stuff

data Upper = Upper Text Loc
  deriving (Eq, Show)

data Lower = Lower Text Loc
  deriving (Eq, Show)

lowerToText :: Lower -> Text
lowerToText (Lower x _) = x
