module Syntax.Concrete where

import Data.Loc
import Data.Text (Text)

data Program = Program [Declaration] [Statement] Loc
  deriving (Show)

data Declaration
  = CondDecl [Condition] Type Loc
  | VarDecl [Variable]  Type Loc
  deriving (Show)

data Statement
  = Skip Loc
  | Abort Loc
  deriving (Show)

--------------------------------------------------------------------------------
-- | Variables and stuff

data Condition = Cond Text Loc
  deriving (Show)

data Variable = Var Text Loc
  deriving (Show)

data Type = Type Text Loc
  deriving (Show)
