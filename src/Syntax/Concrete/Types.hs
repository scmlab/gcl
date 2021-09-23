{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveFoldable #-}

module Syntax.Concrete.Types where

import Data.Loc.Range
import Data.Text (Text)
import GHC.Base (Symbol)
import GHC.Generics (Generic)
import Syntax.Common (Name, Op)
import Prelude hiding (Ordering (..))
import Data.Loc (Located (locOf), Pos, Loc (Loc))

--------------------------------------------------------------------------------

-- | A Token with start & ending Pos
data Token (a :: Symbol) = Token Pos Pos
  deriving (Eq, Show)

instance Located (Token a) where
  locOf (Token l r) = Loc l r

instance Ranged (Token a) where
  rangeOf (Token l r) = Range l r

instance Ranged (Either (Token a) (Token b)) where
  rangeOf (Left x) = rangeOf x
  rangeOf (Right x) = rangeOf x

-- unicode token wraper
type TokQuantStarts = Either (Token "<|") (Token "⟨")

type TokQuantEnds = Either (Token "|>") (Token "⟩")

type TokArrows = Either (Token "->") (Token "→")

-- | A non-empty list of stuff seperated by some delimeter
data SepBy (sep :: Symbol) a = Head a | Delim a (Token sep) (SepBy sep a)
  deriving (Eq, Show, Functor, Foldable)

--------------------------------------------------------------------------------

-- | Program / Declaration / Statement
data Program
  = Program
      [Declaration'] -- constant and variable declarations
      [Stmt] -- main program
  deriving (Eq, Show)

data Declaration
  = ConstDecl (Token "con") DeclType
  | VarDecl (Token "var") DeclType
  -- data T a1 a2 ... = K1 v1 v2 ... | K2 u1 u2 ...
  | TypeDecl (Token "data") QTyCon (Token "=") (SepBy "|" QDCon)
  deriving (Eq, Show)

data BlockDeclaration = BlockDeclaration (Token "{:") [BlockDecl] (Token ":}") deriving (Eq, Show)

data QTyCon = QTyCon Name [Name] deriving (Eq, Show)
data QDCon = QDCon Name [Type] deriving (Eq, Show)

data Stmt
  = Skip Range
  | Abort Range
  | Assign (SepBy "," Name) (Token ":=") (SepBy "," Expr)
  | AAssign Name (Token "[") Expr (Token "]") (Token ":=") Expr
  | Assert (Token "{") Expr (Token "}")
  | LoopInvariant (Token "{") Expr (Token ",") (Token "bnd") (Token ":") Expr (Token "}")
  | Do (Token "do") (SepBy "|" GdCmd) (Token "od")
  | If (Token "if") (SepBy "|" GdCmd) (Token "fi")
  | SpecQM Range -- ? to be rewritten as {!!}
  | Spec (Token "[!") Text (Token "!]")
  | Proof (Token "{-") [ProofAnchor] (Token "-}")
  | Alloc Name (Token ":=") (Token "new") (Token "(") (SepBy "," Expr) (Token ")")
  | HLookup Name (Token ":=") (Token "*") Expr
  | HMutate (Token "*") Expr (Token ":=") Expr
  | Dispose (Token "dispose") Expr
  | Block (Token "|[") Program (Token "]|")
  deriving (Eq, Show)

data GdCmd = GdCmd Expr TokArrows [Stmt] deriving (Eq, Show)
data ProofAnchor = ProofAnchor Text Range deriving (Eq, Show)

--------------------------------------------------------------------------------

-- Low level Declaration wrapper, and synonym types
data DeclBase = DeclBase (SepBy "," Name) (Token ":") Type deriving (Eq, Show)

data DeclProp = DeclProp (Token "{") Expr (Token "}") deriving (Eq, Show)
data DeclType = DeclType DeclBase (Maybe DeclProp) deriving (Eq, Show)
data DeclBody = DeclBody Name [Name] (Token "=") Expr deriving (Eq, Show)

type BlockDeclProp = Either DeclProp Expr
data BlockDeclType = BlockDeclType DeclBase (Maybe BlockDeclProp) deriving (Eq, Show)
type BlockDecl = Either BlockDeclType DeclBody

type Declaration' = Either Declaration BlockDeclaration

--------------------------------------------------------------------------------

-- | Endpoint
data EndpointOpen
  = IncludingOpening (Token "[") Expr
  | ExcludingOpening (Token "(") Expr
  deriving (Eq, Show)

data EndpointClose
  = IncludingClosing Expr (Token "]")
  | ExcludingClosing Expr (Token ")")
  deriving (Eq, Show)

-- | Interval
data Interval = Interval EndpointOpen (Token "..") EndpointClose deriving (Eq, Show)

-- | Base Type
data TBase
  = TInt Range
  | TBool Range
  | TChar Range
  deriving (Eq, Show)

-- | Type
data Type
  = TParen (Token "(") Type (Token ")")
  | TBase TBase
  | TArray (Token "array") Interval (Token "of") Type
  | TFunc Type TokArrows Type
  | TCon QTyCon
  | TVar Name
  deriving (Eq, Show)

--------------------------------------------------------------------------------

-- | Expressions
data Expr
  = Paren (Token "(") Expr (Token ")")
  | Lit Lit
  | Var Name
  | Const Name
  | Op Op
  | Arr Expr (Token "[") Expr (Token "]")
  | App Expr Expr
  | Quant
      TokQuantStarts
      QuantOp'
      [Name]
      (Token ":")
      Expr
      (Token ":")
      Expr
      TokQuantEnds
  deriving (Eq, Show, Generic)

type QuantOp' = Either Op Expr

--------------------------------------------------------------------------------

-- | Literals (Integer / Boolean / Character)
data Lit = LitInt Int Range | LitBool Bool Range | LitChar Char Range
  deriving (Show, Eq, Generic)

--------------------------------------------------------------------------------
