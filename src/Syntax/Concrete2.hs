{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE KindSignatures #-}

module Syntax.Concrete2
  ( module Syntax.Concrete2,
    TBase (..), -- re-exporting from Syntax.Abstract
  )
where

import Data.Aeson (FromJSON, ToJSON)
import Data.Loc
import Data.Map (Map)
import Data.Text.Lazy (Text)
import GHC.Generics (Generic)
import qualified Syntax.Abstract as A
import Syntax.Common
import qualified Syntax.Concrete as C
import qualified Syntax.ConstExpr as ConstExpr
import Syntax.Parser.Lexer (Tok (..))
import Prelude hiding (Ordering (..))

--------------------------------------------------------------------------------

-- | Temporary Typeclass for converting from Syntax.Concrete2 to Syntax.Concrete
class ToConcrete a b | a -> b where
  toConcrete :: a -> b

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

instance ToConcrete Program C.Program where
  toConcrete (Program decls' stmts) =
    let decls = map toConcrete decls'
        letBindings = ConstExpr.pickLetBindings decls
        (globProps, assertions) = ConstExpr.pickGlobals decls
        pre = if null assertions then [] else [C.Assert (C.conjunct assertions) NoLoc]
     in C.Program decls globProps letBindings (pre ++ fmap toConcrete stmts) (decls' <--> stmts)

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

instance ToConcrete Declaration C.Declaration where
  toConcrete (ConstDecl l a _ b) = C.ConstDecl (toConcrete <$> fromSepBy a) (toConcrete b) Nothing (l <--> b)
  toConcrete (ConstDeclWithProp x a _ b _ c r) = C.ConstDecl (toConcrete <$> fromSepBy a) (toConcrete b) (Just $ toConcrete c) (x <--> r)
  toConcrete (VarDecl l a _ b) = C.VarDecl (toConcrete <$> fromSepBy a) (toConcrete b) Nothing (l <--> b)
  toConcrete (VarDeclWithProp x a _ b _ c r) = C.VarDecl (toConcrete <$> fromSepBy a) (toConcrete b) (Just $ toConcrete c) (x <--> r)
  toConcrete (LetDecl l a b _ c) = C.LetDecl (toConcrete a) (fmap nameToText b) (toConcrete c) (l <--> c)

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

instance ToConcrete Stmt C.Stmt where
  toConcrete (Skip l) = C.Skip l
  toConcrete (Abort l) = C.Abort l
  toConcrete (Assign a _ b) = C.Assign (toConcrete <$> fromSepBy a) (toConcrete <$> fromSepBy b) (a <--> b)
  toConcrete (Assert l a r) = C.Assert (toConcrete a) (l <--> r)
  toConcrete (LoopInvariant l a _ _ _ b r) = C.LoopInvariant (toConcrete a) (toConcrete b) (l <--> r)
  toConcrete (Do l a r) = C.Do (toConcrete <$> fromSepBy a) (l <--> r)
  toConcrete (If l a r) = C.If (toConcrete <$> fromSepBy a) (l <--> r)
  toConcrete (SpecQM l) = C.SpecQM l
  toConcrete (Spec l r) = C.Spec (l <--> r)
  toConcrete (Proof l r) = C.Proof (l <--> r)

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

instance ToConcrete GdCmd C.GdCmd where
  toConcrete (GdCmd a _ b) = C.GdCmd (toConcrete a) (fmap toConcrete b) (a <--> b)

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

instance ToConcrete EndpointOpen C.Endpoint where
  toConcrete (IncludingOpening _ a) = C.Including (toConcrete a)
  toConcrete (ExcludingOpening _ a) = C.Excluding (toConcrete a)

instance ToConcrete EndpointClose C.Endpoint where
  toConcrete (IncludingClosing a _) = C.Including (toConcrete a)
  toConcrete (ExcludingClosing a _) = C.Excluding (toConcrete a)

instance Located EndpointOpen where
  locOf (IncludingOpening l e) = l <--> e
  locOf (ExcludingOpening l e) = l <--> e

instance Located EndpointClose where
  locOf (IncludingClosing e l) = e <--> l
  locOf (ExcludingClosing e l) = e <--> l

-- | Interval
data Interval = Interval EndpointOpen (Token 'TokRange) EndpointClose deriving (Eq, Show)

instance ToConcrete Interval C.Interval where
  toConcrete (Interval a _ b) = C.Interval (toConcrete a) (toConcrete b) (a <--> b)

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

instance ToConcrete TBase C.TBase where
  toConcrete (TInt _) = C.TInt
  toConcrete (TBool _) = C.TBool
  toConcrete (TChar _) = C.TChar

-- | Type
data Type
  = TParen (Token 'TokParenOpen) Type (Token 'TokParenClose)
  | TBase TBase
  | TArray (Token 'TokArray) Interval (Token 'TokOf) Type
  | TFunc Type (Either (Token 'TokArrow) (Token 'TokArrowU)) Type
  | TVar Name
  deriving (Eq, Show)

instance ToConcrete Type C.Type where
  toConcrete (TParen _ a _) = toConcrete a
  toConcrete (TBase a) = C.TBase (toConcrete a) (locOf a)
  toConcrete (TArray l a _ b) = C.TArray (toConcrete a) (toConcrete b) (l <--> b)
  toConcrete (TFunc a _ b) = C.TFunc (toConcrete a) (toConcrete b) (a <--> b)
  toConcrete (TVar a) = C.TVar (toConcrete a) (locOf a)

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

instance ToConcrete Expr C.Expr where
  toConcrete x = case x of
    Paren _ a _ -> toConcrete a
    Lit a -> C.Lit (toConcrete a) (locOf x)
    Var a -> C.Var (toConcrete a) (locOf x)
    Const a -> C.Const (toConcrete a) (locOf x)
    Op a -> C.Op (toConcrete a) (locOf x)
    App a b -> C.App (toConcrete a) (toConcrete b) (locOf x)
    Quant _ a b _ c _ d _ -> C.Quant (C.Op (toConcrete a) (locOf a)) (fmap toConcrete b) (toConcrete c) (toConcrete d) (locOf x)

type Subst = Map Text Expr

-- instance ToJSON Expr

-- instance FromJSON Expr

--------------------------------------------------------------------------------

-- | Literals (Integer / Boolean / Character)
data Lit = LitInt Int Loc | LitBool Bool Loc | LitChar Char Loc
  deriving (Show, Eq, Generic)

instance ToConcrete Lit A.Lit where
  toConcrete (LitInt a _) = A.Num a
  toConcrete (LitBool a _) = A.Bol a
  toConcrete (LitChar a _) = A.Chr a

instance Located Lit where
  locOf (LitInt _ l) = l
  locOf (LitBool _ l) = l
  locOf (LitChar _ l) = l

instance ToJSON Lit

instance FromJSON Lit

--------------------------------------------------------------------------------

-- | Names (both UPPER and LOWER cases)
data Name = Name Text Loc
  deriving (Eq, Show, Generic)

instance Located Name where
  locOf (Name _ l) = l

instance ToJSON Name

instance FromJSON Name

instance Ord Name where
  compare (Name a _) (Name b _) = compare a b

-- temp
instance ToConcrete Name C.Name where
  toConcrete (Name a l) = C.Name a l

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

instance ToConcrete Op C.Op where
  toConcrete (EQ _) = C.EQ
  toConcrete (NEQ _) = C.NEQ
  toConcrete (NEQU _) = C.NEQ
  toConcrete (LTE _) = C.LTE
  toConcrete (LTEU _) = C.LTE
  toConcrete (GTE _) = C.GTE
  toConcrete (GTEU _) = C.GTE
  toConcrete (LT _) = C.LT
  toConcrete (GT _) = C.GT
  toConcrete (Implies _) = C.Implies
  toConcrete (ImpliesU _) = C.Implies
  toConcrete (Conj _) = C.Conj
  toConcrete (ConjU _) = C.Conj
  toConcrete (Disj _) = C.Disj
  toConcrete (DisjU _) = C.Disj
  toConcrete (Neg _) = C.Neg
  toConcrete (NegU _) = C.Neg
  toConcrete (Add _) = C.Add
  toConcrete (Sub _) = C.Sub
  toConcrete (Mul _) = C.Mul
  toConcrete (Div _) = C.Div
  toConcrete (Mod _) = C.Mod
  toConcrete (Sum _) = C.Sum
  toConcrete (Forall _) = C.Forall
  toConcrete (Exists _) = C.Exists

instance ToJSON Op

instance FromJSON Op

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

--------------------------------------------------------------------------------

-- | Smart Constructors

-- operators
unary :: Op -> Expr -> Expr
unary op = App (Op op)

binary :: Op -> Expr -> Expr -> Expr
binary op x = App (App (Op op) x)

lt, gt, gte, lte, eqq, conj, disj, implies :: Expr -> Expr -> Expr
lt = binary (LT NoLoc)
gt = binary (GT NoLoc)
gte = binary (GTE NoLoc)
lte = binary (LTE NoLoc)
eqq = binary (EQ NoLoc)
conj = binary (Conj NoLoc)
disj = binary (Disj NoLoc)
implies = binary (Implies NoLoc)

conjunct :: [Expr] -> Expr
conjunct [] = true
conjunct xs = foldl1 conj xs

disjunct :: [Expr] -> Expr
disjunct [] = false
disjunct xs = foldl1 disj xs

imply :: Expr -> Expr -> Expr
imply p = App (App (Op (Implies NoLoc)) p)

predEq :: Expr -> Expr -> Bool
predEq = (==)

-- literals
true :: Expr
true = Lit (LitBool True NoLoc)

false :: Expr
false = Lit (LitBool False NoLoc)

constant :: Text -> Expr
constant x = Const (Name x NoLoc)

variable :: Text -> Expr
variable x = Var (Name x NoLoc)

number :: Int -> Expr
number n = Lit (LitInt n NoLoc)
