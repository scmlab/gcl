module Syntax.Typed where

import Data.Text ( Text )
import Data.Loc ( Loc )
import Data.Loc.Range ( Range )
import Syntax.Abstract.Types ( Lit, Type )
import Syntax.Common.Types ( Name, Op )

-- FIXME:
data TypedProgram = Program [()] -- definitions (the functional language part)
                            [()] -- constant and variable declarations
                            [()] -- global properties
                            [TypedStmt] -- main program
                            Loc

data TypedStmt
  = Skip Loc
  | Abort Loc
  | Assign [Name] [TypedExpr] Loc
  | AAssign TypedExpr TypedExpr TypedExpr Loc
  | Assert TypedExpr Loc
  | LoopInvariant TypedExpr TypedExpr Loc
  | Do [TypedGdCmd] Loc
  | If [TypedGdCmd] Loc
  | Spec Text Range
  | Proof Text Text Range
  -- FIXME: Other constructors.

data TypedGdCmd = TypedGdCmd TypedExpr [TypedStmt] Loc

data TypedExpr
  = Lit Lit Type Loc
  | Var Name Type Loc
  | Const Name Type Loc
  | Op Op Type
  | App TypedExpr TypedExpr Loc
  | Lam Name Type TypedExpr Loc
  -- FIXME: Other constructors.