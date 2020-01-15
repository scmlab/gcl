module Syntax.Concrete
  ( module Syntax.Concrete
  , Op(..)  -- re-export Op(..) from Syntax.Abstract
  ) where

import Data.Loc
import Data.Text.Lazy (Text)
import Prelude hiding (Ordering(..))

import Syntax.Abstract (Op(..))

--------------------------------------------------------------------------------
-- | Program / Declaration / Statement

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

data Lit  = Num Int
          | Bol Bool
          | Chr Char
          deriving (Eq, Show)

data Expr = Lit   Lit       Loc
          | Var   Lower     Loc
          | Const Upper     Loc
          | Op    Op        Loc
          | App   Expr Expr Loc
          | Quant Expr [Lower] Expr Expr Loc
          deriving (Eq, Show)

--------------------------------------------------------------------------------
-- | Variables and stuff

data Upper = Upper Text Loc
  deriving (Eq, Show)

data Lower = Lower Text Loc
  deriving (Eq, Show)

upperToText :: Upper -> Text
upperToText (Upper x _) = x

lowerToText :: Lower -> Text
lowerToText (Lower x _) = x
