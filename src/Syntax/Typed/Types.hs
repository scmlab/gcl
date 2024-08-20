module Syntax.Typed.Types where

import Data.Text ( Text )
import Data.Loc ( Loc )
import Data.Loc.Range ( Range )
import Syntax.Abstract.Types ( Lit(..), Type(..), TBase(..), Pattern, Interval, Kind )
import Syntax.Common.Types ( Name, Op, TypeOp )
import GCL.Common ( Index, TypeInfo )

data Program = Program [Definition] -- definitions (the functional language part)
                       [Declaration] -- constant and variable declarations
                       [Expr] -- global properties
                       [Stmt] -- main program
                       Loc
  deriving (Eq, Show)

data Definition
  = TypeDefn Name [Name] [TypeDefnCtor] Loc
  | FuncDefnSig Name KindedType (Maybe Expr) Loc
  | FuncDefn Name Expr
  deriving (Eq, Show)

data TypeDefnCtor = TypeDefnCtor Name [Type]
  deriving (Eq, Show)

data Declaration
  = ConstDecl [Name] Type (Maybe Expr) Loc
  | VarDecl [Name] Type (Maybe Expr) Loc
  deriving (Eq, Show)

data Stmt
  = Skip Loc
  | Abort Loc
  | Assign [Name] [Expr] Loc
  | AAssign Expr Expr Expr Loc
  | Assert Expr Loc
  | LoopInvariant Expr Expr Loc
  | Do [GdCmd] Loc
  | If [GdCmd] Loc
  | Spec Text Range [(Index, TypeInfo)]
  | Proof Text Text Range
  | Alloc   Name [Expr] Loc    --  p := new (e1,e2,..,en)
  | HLookup Name Expr Loc      --  x := *e
  | HMutate Expr Expr Loc --  *e1 := e2
  | Dispose Expr Loc           --  dispose e
  | Block Program Loc
  deriving (Eq, Show)

data GdCmd = GdCmd Expr [Stmt] Loc
  deriving (Eq, Show)

data Expr
  = Lit Lit Type Loc
  | Var Name Type Loc
  | Const Name Type Loc
  | Op Op Type
  | Chain Chain
  | App Expr Expr Loc
  | Lam Name Type Expr Loc
  | Quant Expr [Name] Expr Expr Loc
  | ArrIdx Expr Expr Loc
  | ArrUpd Expr Expr Expr Loc
  | Case Expr [CaseClause] Loc

  | Subst Expr [(Name, Expr)]
  deriving (Eq, Show)

data CaseClause = CaseClause Pattern Expr
  deriving (Eq, Show)

data Chain
  = Pure Expr
  | More Chain Op Type Expr
  deriving (Eq, Show)

data KindedType
  = TBase TBase Kind Loc
  | TArray Interval KindedType Loc
  | TTuple Int Kind
  | TFunc KindedType KindedType Loc
  | TOp TypeOp Kind
  | TData Name Kind Loc
  | TApp KindedType KindedType Loc
  | TVar Name Kind Loc
  | TMetaVar Name Kind Loc
  deriving (Show, Eq)