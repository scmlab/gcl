module Syntax.Typed where

import Data.Text ( Text )
import Data.Loc ( Loc )
import Data.Loc.Range ( Range )
import qualified Syntax.Abstract.Types as T
import Syntax.Common.Types ( Name, Op, TypeOp )

data TypedProgram = Program [TypedDefinition] -- definitions (the functional language part)
                            [TypedDeclaration] -- constant and variable declarations
                            [TypedExpr] -- global properties
                            [TypedStmt] -- main program
                            Loc
  deriving (Eq, Show)

data TypedDefinition
  = TypeDefn Name [Name] [TypedTypeDefnCtor] Loc
  | FuncDefnSig Name KindedType (Maybe TypedExpr) Loc
  | FuncDefn Name TypedExpr
  deriving (Eq, Show)

data TypedTypeDefnCtor = TypedTypeDefnCtor Name [T.Type]
  deriving (Eq, Show)

data TypedDeclaration
  = ConstDecl [Name] T.Type (Maybe TypedExpr) Loc
  | VarDecl [Name] T.Type (Maybe TypedExpr) Loc
  deriving (Eq, Show)

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
  | Alloc   Name [TypedExpr] Loc    --  p := new (e1,e2,..,en)
  | HLookup Name TypedExpr Loc      --  x := *e
  | HMutate TypedExpr TypedExpr Loc --  *e1 := e2
  | Dispose TypedExpr Loc           --  dispose e
  | Block TypedProgram Loc
  deriving (Eq, Show)

data TypedGdCmd = TypedGdCmd TypedExpr [TypedStmt] Loc
  deriving (Eq, Show)

data TypedExpr
  = Lit T.Lit T.Type Loc
  | Var Name T.Type Loc
  | Const Name T.Type Loc
  | Op Op T.Type
  | Chain TypedChain
  | App TypedExpr TypedExpr Loc
  | Lam Name T.Type TypedExpr Loc
  | Quant TypedExpr [Name] TypedExpr TypedExpr Loc
  | ArrIdx TypedExpr TypedExpr Loc
  | ArrUpd TypedExpr TypedExpr TypedExpr Loc
  | Case TypedExpr [TypedCaseClause] Loc
  deriving (Eq, Show)

data TypedCaseClause = CaseClause T.Pattern TypedExpr
  deriving (Eq, Show)

data TypedChain
  = Pure TypedExpr
  | More TypedChain Op T.Type TypedExpr
  deriving (Eq, Show)

data KindedType
  = TBase T.TBase T.Kind Loc
  | TArray T.Interval KindedType Loc
  | TTuple Int T.Kind
  | TOp TypeOp T.Kind
  | TData Name T.Kind Loc
  | TApp KindedType KindedType Loc
  | TVar Name T.Kind Loc
  | TMetaVar Name T.Kind Loc
  deriving (Show, Eq)

unkind :: KindedType -> T.Type
unkind (TBase base _ loc) = T.TBase base loc
unkind (TArray int kinded loc) = T.TArray int (unkind kinded) loc
unkind (TTuple i _) = T.TTuple i
unkind (TOp op _) = T.TOp op
unkind (TData name _ loc) = T.TData name loc
unkind (TApp kindedTy1 kindedTy2 loc) = T.TApp (unkind kindedTy1) (unkind kindedTy2) loc
unkind (TVar name _ loc) = T.TVar name loc
unkind (TMetaVar name _ loc) = T.TMetaVar name loc