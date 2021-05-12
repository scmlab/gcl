{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE KindSignatures #-}

module Syntax.Concrete where

-- import Syntax.Parser.Lexer (Tok (..))

import Data.Loc (Loc (..), Pos)
import Data.Text (Text)
import GHC.Base (Symbol)
import GHC.Generics (Generic)
import Syntax.Common ( Name, Op, ChainOp, ArithOp )
import Prelude hiding (Ordering (..))

--------------------------------------------------------------------------------

-- | A Token with start & ending Pos
data Token (a :: Symbol) = Token Pos Pos
  deriving (Eq, Show)

-- unicode token wraper
type TokQuantStarts = Either (Token "<|") (Token "⟨")
type TokQuantEnds = Either (Token "|>") (Token "⟩")
type TokArrows = Either (Token "->") (Token "→")

-- | A non-empty list of stuff seperated by commas
data SepBy (sep :: Symbol) a = Head a | Delim a (Token sep) (SepBy sep a)
  deriving (Eq, Show)

--------------------------------------------------------------------------------

-- | Program / Declaration / Statement
data Program
  = Program
      [Declaration'] -- constant and variable declarations
      [Stmt] -- main program
  deriving (Eq, Show)

data Declaration
  = ConstDecl (Token "con") Decl
  | ConstDeclWithProp (Token "con") Decl DeclProp
  | VarDecl (Token "var") Decl
  | VarDeclWithProp (Token "var") Decl DeclProp
  | LetDecl (Token "let") DeclBody
  deriving (Eq, Show)

data BlockDeclaration = BlockDeclaration (Token "{:") [BlockDecl] (Token ":}") deriving (Eq, Show)

data Stmt
  = Skip Loc
  | Abort Loc
  | Assign (SepBy "," Name) (Token ":=") (SepBy "," Expr)
  | Assert (Token "{") Expr (Token "}")
  | LoopInvariant (Token "{") Expr (Token ",") (Token "bnd") (Token ":") Expr (Token "}")
  | Do (Token "do") (SepBy "|" GdCmd) (Token "od")
  | If (Token "if") (SepBy "|" GdCmd) (Token "fi")
  | SpecQM Loc -- ? to be rewritten as {!!} by the frontend
  | Spec (Token "[!") Text (Token "!]")
  | Proof (Token "{-") (Token "-}")
  deriving (Eq, Show)

data GdCmd = GdCmd Expr TokArrows [Stmt] deriving (Eq, Show)

--------------------------------------------------------------------------------

-- Low level Declaration wrapper, and synonym types
data Decl = Decl (SepBy "," Name) (Token ":") Type deriving (Eq, Show)

data DeclProp = DeclProp (Token "{") Expr (Token "}") deriving (Eq, Show)
type DeclProp' = Either DeclProp Expr

data DeclBody = DeclBody Name [Name] (Token "=") Expr deriving (Eq, Show)

data BlockDecl = BlockDecl Decl (Maybe DeclProp') (Maybe DeclBody) deriving (Eq, Show)

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
  = TInt Loc
  | TBool Loc
  | TChar Loc
  deriving (Eq, Show)

-- | Type
data Type
  = TParen (Token "(") Type (Token ")")
  | TBase TBase
  | TArray (Token "array") Interval (Token "of") Type
  | TFunc Type TokArrows Type
  | TVar Name
  deriving (Eq, Show)

--------------------------------------------------------------------------------

-- | Expressions
data Expr
  = Paren (Token "(") Expr (Token ")")
  | Lit Lit
  | Var Name
  | Const Name
  | Op ArithOp
  | Chain Expr ChainOp Expr -- Left Associative
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
data Lit = LitInt Int Loc | LitBool Bool Loc | LitChar Char Loc
  deriving (Show, Eq, Generic)

--------------------------------------------------------------------------------