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

data Program = Program [Definition]       -- definitions (the functional language part)
                                    [Declaration]     -- constant and variable declarations
                                                  [Expr]            -- global properties
                                                         [Stmt]            -- main program
                                                                Loc
  deriving (Eq, Show)

--------------------------------------------------------------------------------
-- | Definition (the functional language part)
data Definition =
    TypeDefn Name [Name] [TypeDefnCtor] Loc
    | FuncDefnSig Name Type (Maybe Expr) Loc
    | FuncDefn Name [Expr]
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
  | Proof Text Text Range
    -- pointer operations
  | Alloc   Name [Expr] Loc    --  p := new (e1,e2,..,en)
  | HLookup Name Expr Loc      --  x := *e
  | HMutate Expr Expr Loc      --  *e1 := e2
  | Dispose Expr Loc           --  dispose e
  | Block Program Loc

  deriving (Eq, Show)

data GdCmd = GdCmd Expr [Stmt] Loc
  deriving (Eq, Show)
-- data ProofAnchor = ProofAnchor Text Range
--   deriving (Eq, Ord, Show)


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
  -- TTuple has no srcloc info because it has no conrete syntax at the moment 
  | TTuple [Type]
  | TFunc Type Type Loc
  | TApp Type Type Loc
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
  | Func Name (NonEmpty FuncClause) Loc
  -- Tuple has no srcloc info because it has no conrete syntax at the moment 
  | Tuple [Expr]
  | Quant Expr [Name] Expr Expr Loc
  -- The innermost part of a Redex 
  -- should look something like `P [ x \ a ] [ y \ b ]`
  | RedexKernel
    Name -- the variable to be substituted
    Expr -- the expression for substituting the variable
    (Set Name) -- free variables in that expression
               -- NOTE, the expression may be some definition like "P",
              --  in that case, the free variables should be that of after it's been expanded
    (NonEmpty Mapping) 
      -- a list of mappings of substitutions to be displayed to users (the `[ x \ a ] [ y \ b ]` part)
      -- The order is reflected.
  -- The outermost part of a Redex
  | RedexShell
      Int -- for identifying Redexes in frontend-backend interactions 
      Expr -- should either be `RedexKernel` or `App (App (App ... RedexKernel arg0) ... argn`
  | ArrIdx Expr Expr Loc
  | ArrUpd Expr Expr Expr Loc
  | Case Expr [CaseClause] Loc
  deriving (Eq, Show, Generic)

-- QuantOp' seems not being used at current version of abstract?
type QuantOp' = Either Op Expr

type Mapping = Map Text Expr

--------------------------------------------------------------------------------
-- | Pattern matching

-- pattern -> expr 
data CaseClause = CaseClause Pattern Expr
  deriving (Eq, Show, Generic)

-- pattern0 pattern1 pattern2 ... -> expr 
data FuncClause = FuncClause [Pattern] Expr
  deriving (Eq, Show, Generic)

data Pattern
  = PattLit Lit
  | PattBinder Name -- binder
  | PattWildcard Range -- matches anything
  | PattConstructor Name [Pattern] -- destructs a constructor
  deriving (Eq, Show, Generic)

extractBinder :: Pattern -> [Name]
extractBinder (PattLit      _      ) = []
extractBinder (PattBinder   x      ) = [x]
extractBinder (PattWildcard _      ) = []
extractBinder (PattConstructor _ xs) = xs >>= extractBinder

----------------------------------------------------------------

-- | Literals
data Lit = Num Int | Bol Bool | Chr Char | Emp
  deriving (Show, Eq, Generic)

----------------------------------------------------------------
