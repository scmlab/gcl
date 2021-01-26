{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE KindSignatures #-}

module Syntax.Concrete where

import Data.Loc ( Loc(..), Pos, (<-->), Located(locOf) )
import Data.Map (Map)
import Data.Text.Lazy (Text)
import GHC.Generics (Generic)
import Syntax.Common (Fixity(..))
import qualified Syntax.Abstract as A
import qualified Syntax.ConstExpr as ConstExpr
import Syntax.Parser.Lexer (Tok (..))
import Prelude hiding (Ordering (..))

--------------------------------------------------------------------------------

-- | Typeclass for converting from Syntax.Concrete to Syntax.Abstract
class ToAbstract a b | a -> b where
  toAbstract :: a -> b

--------------------------------------------------------------------------------

-- | A Token with start & ending Pos
data Token (a :: Tok) = Token Pos Pos
  deriving (Eq, Show)

instance Located (Token a) where
  locOf (Token l r) = Loc l r

instance (Located a, Located b) => Located (Either a b) where
  locOf (Left x) = locOf x
  locOf (Right x) = locOf x

-- | A non-empty list of stuff seperated by commas
data SepBy (sep :: Tok) a = Head a | Delim a (Token sep) (SepBy sep a)
  deriving (Eq, Show)

fromSepBy :: SepBy sep a -> [a]
fromSepBy (Head a) = [a]
fromSepBy (Delim a _ as) = a : fromSepBy as

instance Located a => Located (SepBy sep a) where
  locOf (Head a) = locOf a
  locOf (Delim a _ as) = a <--> locOf as

--------------------------------------------------------------------------------

-- | Program / Declaration / Statement
data Program
  = Program
      [Declaration] -- constant and variable declarations
      [Stmt] -- main program
  deriving (Eq, Show)

instance ToAbstract Program A.Program where
  toAbstract (Program decls' stmts) =
    let decls = map toAbstract decls'
        letBindings = ConstExpr.pickLetBindings decls
        (globProps, assertions) = ConstExpr.pickGlobals decls
        pre = if null assertions then [] else [A.Assert (A.conjunct assertions) NoLoc]
     in A.Program decls globProps letBindings (pre ++ fmap toAbstract stmts) (decls' <--> stmts)

instance Located Program where
  locOf (Program a b) = a <--> b

type Defns = Map Text Expr

data Declaration
  = ConstDecl (Token 'TokCon) (SepBy 'TokComma Name) (Token 'TokColon) Type
  | ConstDeclWithProp (Token 'TokCon) (SepBy 'TokComma Name) (Token 'TokColon) Type (Token 'TokBraceOpen) Expr (Token 'TokBraceClose)
  | VarDecl (Token 'TokVar) (SepBy 'TokComma Name) (Token 'TokColon) Type
  | VarDeclWithProp (Token 'TokVar) (SepBy 'TokComma Name) (Token 'TokColon) Type (Token 'TokBraceOpen) Expr (Token 'TokBraceClose)
  | LetDecl (Token 'TokLet) Name [Name] (Token 'TokEQ) Expr
  deriving (Eq, Show)

instance ToAbstract Declaration A.Declaration where
  toAbstract (ConstDecl l a _ b) = A.ConstDecl (toAbstract <$> fromSepBy a) (toAbstract b) Nothing (l <--> b)
  toAbstract (ConstDeclWithProp x a _ b _ c r) = A.ConstDecl (toAbstract <$> fromSepBy a) (toAbstract b) (Just $ toAbstract c) (x <--> r)
  toAbstract (VarDecl l a _ b) = A.VarDecl (toAbstract <$> fromSepBy a) (toAbstract b) Nothing (l <--> b)
  toAbstract (VarDeclWithProp x a _ b _ c r) = A.VarDecl (toAbstract <$> fromSepBy a) (toAbstract b) (Just $ toAbstract c) (x <--> r)
  toAbstract (LetDecl l a b _ c) = A.LetDecl (toAbstract a) (fmap nameToText b) (toAbstract c) (l <--> c)

instance Located Declaration where
  locOf (ConstDecl l _ _ r) = l <--> r
  locOf (ConstDeclWithProp l _ _ _ _ _ r) = l <--> r
  locOf (VarDecl l _ _ r) = l <--> r
  locOf (VarDeclWithProp l _ _ _ _ _ r) = l <--> r
  locOf (LetDecl l _ _ _ r) = l <--> r

data Stmt
  = Skip Loc
  | Abort Loc
  | Assign (SepBy 'TokComma Name) (Token 'TokAssign) (SepBy 'TokComma Expr)
  | Assert (Token 'TokBraceOpen) Expr (Token 'TokBraceClose)
  | LoopInvariant (Token 'TokBraceOpen) Expr (Token 'TokComma) (Token 'TokBnd) (Token 'TokColon) Expr (Token 'TokBraceClose)
  | Do (Token 'TokDo) (SepBy 'TokGuardBar GdCmd) (Token 'TokOd)
  | If (Token 'TokIf) (SepBy 'TokGuardBar GdCmd) (Token 'TokFi)
  | SpecQM Loc -- ? to be rewritten as {!!} by the frontend
  | Spec (Token 'TokSpecOpen) (Token 'TokSpecClose)
  | Proof (Token 'TokProofOpen) (Token 'TokProofClose)
  deriving (Eq, Show)

instance ToAbstract Stmt A.Stmt where
  toAbstract (Skip l) = A.Skip l
  toAbstract (Abort l) = A.Abort l
  toAbstract (Assign a _ b) = A.Assign (toAbstract <$> fromSepBy a) (toAbstract <$> fromSepBy b) (a <--> b)
  toAbstract (Assert l a r) = A.Assert (toAbstract a) (l <--> r)
  toAbstract (LoopInvariant l a _ _ _ b r) = A.LoopInvariant (toAbstract a) (toAbstract b) (l <--> r)
  toAbstract (Do l a r) = A.Do (toAbstract <$> fromSepBy a) (l <--> r)
  toAbstract (If l a r) = A.If (toAbstract <$> fromSepBy a) (l <--> r)
  toAbstract (SpecQM l) = A.SpecQM l
  toAbstract (Spec l r) = A.Spec (l <--> r)
  toAbstract (Proof l r) = A.Proof (l <--> r)

instance Located Stmt where
  locOf (Skip l) = l
  locOf (Abort l) = l
  locOf (Assign l _ r) = l <--> r
  locOf (Assert l _ r) = l <--> r
  locOf (LoopInvariant l _ _ _ _ _ r) = l <--> r
  locOf (Do l _ r) = l <--> r
  locOf (If l _ r) = l <--> r
  locOf (SpecQM l) = l
  locOf (Spec l r) = l <--> r
  locOf (Proof l r) = l <--> r

data GdCmd = GdCmd Expr (Either (Token 'TokArrow) (Token 'TokArrowU)) [Stmt] deriving (Eq, Show)

instance ToAbstract GdCmd A.GdCmd where
  toAbstract (GdCmd a _ b) = A.GdCmd (toAbstract a) (fmap toAbstract b) (a <--> b)

--------------------------------------------------------------------------------

-- | Interval
data EndpointOpen
  = IncludingOpening (Token 'TokBracketOpen) Expr
  | ExcludingOpening (Token 'TokParenOpen) Expr
  deriving (Eq, Show)

data EndpointClose
  = IncludingClosing Expr (Token 'TokBracketClose)
  | ExcludingClosing Expr (Token 'TokParenClose)
  deriving (Eq, Show)

instance ToAbstract EndpointOpen A.Endpoint where
  toAbstract (IncludingOpening _ a) = A.Including (toAbstract a)
  toAbstract (ExcludingOpening _ a) = A.Excluding (toAbstract a)

instance ToAbstract EndpointClose A.Endpoint where
  toAbstract (IncludingClosing a _) = A.Including (toAbstract a)
  toAbstract (ExcludingClosing a _) = A.Excluding (toAbstract a)

instance Located EndpointOpen where
  locOf (IncludingOpening l e) = l <--> e
  locOf (ExcludingOpening l e) = l <--> e

instance Located EndpointClose where
  locOf (IncludingClosing e l) = e <--> l
  locOf (ExcludingClosing e l) = e <--> l

-- | Interval
data Interval = Interval EndpointOpen (Token 'TokRange) EndpointClose deriving (Eq, Show)

instance ToAbstract Interval A.Interval where
  toAbstract (Interval a _ b) = A.Interval (toAbstract a) (toAbstract b) (a <--> b)

instance Located Interval where
  locOf (Interval l _ r) = l <--> r

-- | Base Type
data TBase 
  = TInt Loc
  | TBool Loc 
  | TChar Loc 
  deriving (Eq, Show)

instance Located TBase where
  locOf (TInt l) = l
  locOf (TBool l) = l
  locOf (TChar l) = l

instance ToAbstract TBase A.TBase where
  toAbstract (TInt _) = A.TInt
  toAbstract (TBool _) = A.TBool
  toAbstract (TChar _) = A.TChar

-- | Type
data Type
  = TParen (Token 'TokParenOpen) Type (Token 'TokParenClose)
  | TBase TBase
  | TArray (Token 'TokArray) Interval (Token 'TokOf) Type
  | TFunc Type (Either (Token 'TokArrow) (Token 'TokArrowU)) Type
  | TVar Name
  deriving (Eq, Show)

instance ToAbstract Type A.Type where
  toAbstract (TParen _ a _) = toAbstract a
  toAbstract (TBase a) = A.TBase (toAbstract a) (locOf a)
  toAbstract (TArray l a _ b) = A.TArray (toAbstract a) (toAbstract b) (l <--> b)
  toAbstract (TFunc a _ b) = A.TFunc (toAbstract a) (toAbstract b) (a <--> b)
  toAbstract (TVar a) = A.TVar (toAbstract a) (locOf a)

instance Located Type where
  locOf (TParen l _ r) = l <--> r
  locOf (TBase a) = locOf a
  locOf (TArray l _ _ r) = l <--> r
  locOf (TFunc l _ r) = l <--> r
  locOf (TVar x) = locOf x

--------------------------------------------------------------------------------

-- | Expressions
data Expr
  = Paren (Token 'TokParenOpen) Expr (Token 'TokParenClose)
  | Lit Lit
  | Var Name
  | Const Name
  | Op Op
  | App Expr Expr
  | Quant
      (Either (Token 'TokQuantOpen) (Token 'TokQuantOpenU))
      Op
      [Name]
      (Token 'TokColon)
      Expr
      (Token 'TokColon)
      Expr
      (Either (Token 'TokQuantClose) (Token 'TokQuantCloseU))
  deriving (Eq, Show, Generic)

instance Located Expr where
  locOf (Paren l _ r) = l <--> r
  locOf (Lit x) = locOf x
  locOf (Var x) = locOf x
  locOf (Const x) = locOf x
  locOf (Op x) = locOf x
  locOf (App x y) = x <--> y
  locOf (Quant l _ _ _ _ _ _ r) = l <--> r

instance ToAbstract Expr A.Expr where
  toAbstract x = case x of
    Paren _ a _ -> toAbstract a
    Lit a -> A.Lit (toAbstract a) (locOf x)
    Var a -> A.Var (toAbstract a) (locOf x)
    Const a -> A.Const (toAbstract a) (locOf x)
    Op a -> A.Op (toAbstract a) (locOf x)
    App a b -> A.App (toAbstract a) (toAbstract b) (locOf x)
    Quant _ a b _ c _ d _ -> A.Quant (A.Op (toAbstract a) (locOf a)) (fmap toAbstract b) (toAbstract c) (toAbstract d) (locOf x)

--------------------------------------------------------------------------------

-- | Literals (Integer / Boolean / Character)
data Lit = LitInt Int Loc | LitBool Bool Loc | LitChar Char Loc
  deriving (Show, Eq, Generic)

instance ToAbstract Lit A.Lit where
  toAbstract (LitInt a _) = A.Num a
  toAbstract (LitBool a _) = A.Bol a
  toAbstract (LitChar a _) = A.Chr a

instance Located Lit where
  locOf (LitInt _ l) = l
  locOf (LitBool _ l) = l
  locOf (LitChar _ l) = l

--------------------------------------------------------------------------------

-- | Names (both UPPER and LOWER cases)
data Name = Name Text Loc
  deriving (Eq, Show, Generic)

instance Located Name where
  locOf (Name _ l) = l

instance ToAbstract Name A.Name where
  toAbstract (Name a l) = A.Name a l

nameToText :: Name -> Text
nameToText (Name x _) = x

--------------------------------------------------------------------------------

-- | Operators
data Op
  = -- binary relations
    EQ Loc
  | NEQ Loc
  | NEQU Loc
  | LTE Loc
  | LTEU Loc
  | GTE Loc
  | GTEU Loc
  | LT Loc
  | GT Loc
  | -- logic operators
    Implies Loc
  | ImpliesU Loc
  | Conj Loc
  | ConjU Loc
  | Disj Loc
  | DisjU Loc
  | -- arithmetics
    Neg Loc
  | NegU Loc
  | Add Loc
  | Sub Loc
  | Mul Loc
  | Div Loc
  | Mod Loc
  | -- For Quant
    Sum Loc
  | Forall Loc
  | Exists Loc
  deriving (Show, Eq, Generic)

instance ToAbstract Op A.Op where
  toAbstract (EQ _) = A.EQ
  toAbstract (NEQ _) = A.NEQ
  toAbstract (NEQU _) = A.NEQ
  toAbstract (LTE _) = A.LTE
  toAbstract (LTEU _) = A.LTE
  toAbstract (GTE _) = A.GTE
  toAbstract (GTEU _) = A.GTE
  toAbstract (LT _) = A.LT
  toAbstract (GT _) = A.GT
  toAbstract (Implies _) = A.Implies
  toAbstract (ImpliesU _) = A.Implies
  toAbstract (Conj _) = A.Conj
  toAbstract (ConjU _) = A.Conj
  toAbstract (Disj _) = A.Disj
  toAbstract (DisjU _) = A.Disj
  toAbstract (Neg _) = A.Neg
  toAbstract (NegU _) = A.Neg
  toAbstract (Add _) = A.Add
  toAbstract (Sub _) = A.Sub
  toAbstract (Mul _) = A.Mul
  toAbstract (Div _) = A.Div
  toAbstract (Mod _) = A.Mod
  toAbstract (Sum _) = A.Sum
  toAbstract (Forall _) = A.Forall
  toAbstract (Exists _) = A.Exists

instance Located Op where
  locOf (Implies l) = l
  locOf (ImpliesU l) = l
  locOf (Disj l) = l
  locOf (DisjU l) = l
  locOf (Conj l) = l
  locOf (ConjU l) = l
  locOf (Neg l) = l
  locOf (NegU l) = l
  locOf (EQ l) = l
  locOf (NEQ l) = l
  locOf (NEQU l) = l
  locOf (LTE l) = l
  locOf (LTEU l) = l
  locOf (GTE l) = l
  locOf (GTEU l) = l
  locOf (LT l) = l
  locOf (GT l) = l
  locOf (Add l) = l
  locOf (Sub l) = l
  locOf (Mul l) = l
  locOf (Div l) = l
  locOf (Mod l) = l
  locOf (Sum l) = l
  locOf (Exists l) = l
  locOf (Forall l) = l

classify :: Op -> Fixity
classify (Implies _) = InfixR 1
classify (ImpliesU _) = InfixR 1
classify (Disj _) = InfixL 2
classify (DisjU _) = InfixL 2
classify (Conj _) = InfixL 3
classify (ConjU _) = InfixL 3
classify (Neg _) = Prefix 4
classify (NegU _) = Prefix 4
classify (EQ _) = Infix 5
classify (NEQ _) = Infix 6
classify (NEQU _) = Infix 6
classify (LTE _) = Infix 6
classify (LTEU _) = Infix 6
classify (GTE _) = Infix 6
classify (GTEU _) = Infix 6
classify (LT _) = Infix 6
classify (GT _) = Infix 6
classify (Add _) = InfixL 7
classify (Sub _) = InfixL 7
classify (Mul _) = InfixL 8
classify (Div _) = InfixL 8
classify (Mod _) = InfixL 9
classify (Sum _) = Prefix 5
classify (Exists _) = Prefix 6
classify (Forall _) = Prefix 7
