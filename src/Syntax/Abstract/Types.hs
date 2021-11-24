{-# LANGUAGE DeriveGeneric #-}

module Syntax.Abstract.Types where

import           Data.List.NonEmpty             ( NonEmpty )
import           Data.Loc                       ( Loc )
import           Data.Loc.Range                 ( Range )
import           Data.Map                       ( Map )
import           Data.Set                       ( Set )
import           Data.Text                      ( Text )
import           GHC.Generics                   ( Generic )
import           Prelude                 hiding ( Ordering(..) )
import           Syntax.Common                  ( Name
                                                , Op
                                                )
--------------------------------------------------------------------------------

type Const = Text

type Var = Text

type TypeVar = Text

--------------------------------------------------------------------------------
-- | Program

data Program = Program Definitions       -- definitions (the functional language part)
                                   [Declaration]     -- constant and variable declarations
                                                 [Expr]            -- global properties
                                                        [Stmt]            -- main program
                                                               Loc
  deriving (Eq, Show)

--------------------------------------------------------------------------------
-- | Definitions (the functional language part)

data Definitions = Definitions
  { defnTypes    :: Map Name TypeDefn
  , defnFuncSigs :: Map Name FuncDefnSig
  , defnFuncs    :: Map Name [Expr]
  }
  deriving (Eq, Show)

instance Semigroup Definitions where
  Definitions a b c <> Definitions d e f =
    Definitions (a <> d) (b <> e) (c <> f)

instance Monoid Definitions where
  mempty = Definitions mempty mempty mempty

-- function definition
data FuncDefn = FuncDefn
  { funcName    :: Name
  , funcClauses :: [([Name], Expr)]
  , funcLoc     :: Loc
  }
  deriving (Eq, Show)

-- type signature of function definition
data FuncDefnSig = FuncDefnSig Name Type (Maybe Expr) Loc
  deriving (Eq, Show)

-- type definition
data TypeDefn = TypeDefn Name [Name] [TypeDefnCtor] Loc
  deriving (Eq, Show)

-- constructor of type definition
data TypeDefnCtor = TypeDefnCtor Name [Type]
  deriving (Eq, Show)

--------------------------------------------------------------------------------

-- | Declaration
data Declaration
  = ConstDecl [Name] Type (Maybe Expr) Loc
  | VarDecl [Name] Type (Maybe Expr) Loc
  deriving (Eq, Show)

--------------------------------------------------------------------------------

data Stmt
  = Skip Loc
  | Abort Loc
  | Assign [Name] [Expr] Loc
  | AAssign Expr Expr Expr Loc
  | Assert Expr Loc
  | LoopInvariant Expr Expr Loc
  | Do [GdCmd] Loc
  | If [GdCmd] Loc
  | Spec Text Range
  | Proof [ProofAnchor] Loc
    -- pointer operations
  | Alloc   Name [Expr] Loc    --  p := new (e1,e2,..,en)
  | HLookup Name Expr Loc      --  x := *e
  | HMutate Expr Expr Loc      --  *e1 := e2
  | Dispose Expr Loc           --  dispose e
  | Block Program Loc

  deriving (Eq, Show)

data GdCmd = GdCmd Expr [Stmt] Loc
  deriving (Eq, Show)
data ProofAnchor = ProofAnchor Text Range
  deriving (Eq, Ord, Show)

--------------------------------------------------------------------------------

-- | Endpoint
data Endpoint = Including Expr | Excluding Expr deriving (Eq, Show, Generic)

-- | Interval
data Interval = Interval Endpoint Endpoint Loc
  deriving (Eq, Show, Generic)

-- | Base Types
data TBase = TInt | TBool | TChar
  deriving (Show, Eq, Generic)

-- | Types
data Type
  = TBase TBase Loc
  | TArray Interval Type Loc
  | TFunc Type Type Loc
  | TCon Name [Name] Loc
  | TVar Name Loc
  | TMetaVar Name
  deriving (Eq, Show, Generic)

--------------------------------------------------------------------------------

-- | Expressions
data Expr
  = Lit Lit Loc
  | Var Name Loc
  | Const Name Loc
  | Op Op
  | App Expr Expr Loc
  | Lam Name Expr Loc
  | Quant Expr [Name] Expr Expr Loc
  | RedexStem -- the inner part of a Redex `name [ v \ e ... ] [ v \ e ... ]`
    Name -- the name to be substituted
    Expr -- the expression for substituting the name
    (Set Name)
     -- free variables in that expression
               -- NOTE, the expression may be some definition like "P",
              --  in that case, the free variables should be that of after it's been expanded
    (NonEmpty Mapping) -- mapping of substitution to be displayed to users
  | Redex Redex
  | ArrIdx Expr Expr Loc
  | ArrUpd Expr Expr Expr Loc
  | Case Expr [CaseConstructor] Loc
  deriving (Eq, Show, Generic)

type QuantOp' = Either Op Expr

type Mapping = Map Text Expr

data Redex = Rdx
  { redexID   :: Int
  , redexExpr :: Expr
  }
  deriving (Eq, Show, Generic)

--------------------------------------------------------------------------------
-- | Pattern matching

data CaseConstructor = CaseConstructor Pattern Expr
  deriving (Eq, Show, Generic)

data Pattern
  = PattLit Lit
  | PattBinder Name -- binder
  | PattWildcard Range -- matches anything
  | PattConstructor Name [Pattern] -- destructs a constructor
  deriving (Eq, Show, Generic)

----------------------------------------------------------------

-- | Literals
data Lit = Num Int | Bol Bool | Chr Char | Emp
  deriving (Show, Eq, Generic)

----------------------------------------------------------------
