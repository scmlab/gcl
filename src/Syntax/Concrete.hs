module Syntax.Concrete where

import Data.Loc

data Program = Program [Declaration] [Statement] Loc

data Declaration
  = Condition Loc
  deriving (Show)

data Statement
  = Skip Loc
  | Abort Loc
  deriving (Show)
