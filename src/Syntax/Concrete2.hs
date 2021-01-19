{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FunctionalDependencies #-}

module Syntax.Concrete2 
  ( module Syntax.Concrete2,
    Op (..),
    TBase (..),
    Lit (..), -- re-exporting from Syntax.Abstract
  )
where

import Data.Aeson
import Data.Loc
import Data.Map (Map)
import Data.Text.Lazy (Text)
import GHC.Generics (Generic)
import Syntax.Abstract
  ( Lit (..),
    Op (..),
    TBase (..),
  )
import qualified Syntax.Concrete as C
import Prelude hiding (Ordering (..))

--------------------------------------------------------------------------------

-- | Temporary Typeclass for converting from Syntax.Concrete2 to Syntax.Concrete
class ToConcrete a b | a -> b where
  toConcrete :: a -> b

--------------------------------------------------------------------------------

-- | Program / Declaration / Statement
data Program
  = Program
      [Declaration] -- constant and variable declarations
      [Expr] -- global properties
      Defns -- let bindings
      [Stmt] -- main program
      Loc
  deriving (Eq, Show)

instance ToConcrete Program C.Program where
  toConcrete (Program a b c d l) =
    C.Program (map toConcrete a) (fmap toConcrete b) (fmap toConcrete c) (fmap toConcrete d) l

instance Located Program where
  locOf (Program _ _ _ _ l) = l

type Defns = Map Text Expr

data Declaration
  = ConstDecl [Name] Type (Maybe Expr) Loc
  | VarDecl [Name] Type (Maybe Expr) Loc
  | LetDecl Name [Text] Expr Loc
  deriving (Eq, Show)

instance ToConcrete Declaration C.Declaration where
  toConcrete (ConstDecl a b c l) = C.ConstDecl (fmap toConcrete a) (toConcrete b) (fmap toConcrete c) l
  toConcrete (VarDecl a b c l) = C.VarDecl (fmap toConcrete a) (toConcrete b) (fmap toConcrete c) l
  toConcrete (LetDecl a b c l) = C.LetDecl (toConcrete a) b (toConcrete c) l

data Stmt
  = Skip Loc
  | Abort Loc
  | Assign [Name] [Expr] Loc
  | Assert Expr Loc
  | LoopInvariant Expr Expr Loc
  | Do [GdCmd] Loc
  | If [GdCmd] Loc
  | SpecQM Loc -- ? to be rewritten as {!!} by the frontend
  | Spec Loc
  | Proof Loc
  deriving (Eq, Show)

instance ToConcrete Stmt C.Stmt where
  toConcrete (Skip l) = C.Skip l
  toConcrete (Abort l) = C.Abort l
  toConcrete (Assign a b l) = C.Assign (fmap toConcrete a) (fmap toConcrete b) l
  toConcrete (Assert a l) = C.Assert (toConcrete a) l
  toConcrete (LoopInvariant a b l) = C.LoopInvariant (toConcrete a) (toConcrete b) l
  toConcrete (Do a l) = C.Do (fmap toConcrete a) l
  toConcrete (If a l) = C.If (fmap toConcrete a) l
  toConcrete (SpecQM l) = C.SpecQM l
  toConcrete (Spec l) = C.Spec l
  toConcrete (Proof l) = C.Proof l

instance Located Stmt where
  locOf (Skip l) = l
  locOf (Abort l) = l
  locOf (Assign _ _ l) = l
  locOf (Assert _ l) = l
  locOf (LoopInvariant _ _ l) = l
  locOf (Do _ l) = l
  locOf (If _ l) = l
  locOf (SpecQM l) = l
  locOf (Spec l) = l
  locOf (Proof l) = l

data GdCmd = GdCmd Expr [Stmt] Loc deriving (Eq, Show)

instance ToConcrete GdCmd C.GdCmd where
  toConcrete (GdCmd a b l) = C.GdCmd (toConcrete a) (fmap toConcrete b) l
  
extractAssertion :: Declaration -> Maybe Expr
extractAssertion (ConstDecl _ _ e _) = e
extractAssertion (VarDecl _ _ e _) = e
extractAssertion LetDecl {} = Nothing

extractLetBinding :: Declaration -> Maybe (Text, Expr)
extractLetBinding ConstDecl {} = Nothing
extractLetBinding VarDecl {} = Nothing
extractLetBinding (LetDecl c a e _) = Just (nameToText c, wrapLam a e)

getGuards :: [GdCmd] -> [Expr]
getGuards = fst . unzipGdCmds

unzipGdCmds :: [GdCmd] -> ([Expr], [[Stmt]])
unzipGdCmds = unzip . map (\(GdCmd x y _) -> (x, y))

--------------------------------------------------------------------------------

-- | Types
data Endpoint = Including Expr | Excluding Expr deriving (Eq, Show)

instance ToConcrete Endpoint C.Endpoint where
  toConcrete (Including a) = C.Including (toConcrete a)
  toConcrete (Excluding a) = C.Excluding (toConcrete a)
  
instance Located Endpoint where
  locOf (Including e) = locOf e
  locOf (Excluding e) = locOf e

data Interval = Interval Endpoint Endpoint Loc deriving (Eq, Show)

instance ToConcrete Interval C.Interval where
  toConcrete (Interval a b l) = C.Interval (toConcrete a) (toConcrete b) l

instance Located Interval where
  locOf (Interval _ _ l) = l

data Type
  = TBase TBase Loc
  | TArray Interval Type Loc
  | TFunc Type Type Loc
  | TVar Name Loc
  deriving (Eq, Show)

instance ToConcrete Type C.Type where
  toConcrete (TBase a l) = C.TBase a l
  toConcrete (TArray a b l) = C.TArray (toConcrete a) (toConcrete b) l
  toConcrete (TFunc a b l) = C.TFunc (toConcrete a) (toConcrete b) l
  toConcrete (TVar a l) = C.TVar (toConcrete a) l
  
instance Located Type where
  locOf (TBase _ l) = l
  locOf (TArray _ _ l) = l
  locOf (TFunc _ _ l) = l
  locOf (TVar _ l) = l

instance Relocatable Type where
  reloc l (TBase base _) = TBase base l
  reloc l (TArray i s _) = TArray i s l
  reloc l (TFunc s t _) = TFunc s t l
  reloc l (TVar x _) = TVar x l

--------------------------------------------------------------------------------

-- | Expressions
data Expr
  = Lit Lit Loc
  | Var Name Loc
  | Const Name Loc
  | Op Op Loc
  | App Expr Expr Loc
  | Lam Text Expr Loc
  | Hole Loc
  | Quant Expr [Name] Expr Expr Loc
  | Subst Expr Subst -- internal. Location not necessary?
  deriving (Eq, Show, Generic)

instance ToConcrete Expr C.Expr where
  toConcrete (Lit a l) = C.Lit a l
  toConcrete (Var a l) = C.Var (toConcrete a) l
  toConcrete (Const a l) = C.Const (toConcrete a) l
  toConcrete (Op a l) = C.Op a l
  toConcrete (App a b l) = C.App (toConcrete a) (toConcrete b) l
  toConcrete (Lam a b l) = C.Lam a (toConcrete b) l
  toConcrete (Hole l) = C.Hole l
  toConcrete (Quant a b c d l) = C.Quant (toConcrete a) (fmap toConcrete b) (toConcrete c) (toConcrete d) l
  toConcrete (Subst a b) = C.Subst (toConcrete a) (fmap toConcrete b)

instance Relocatable Expr where
  reloc l (Var x _) = Var x l
  reloc l (Const x _) = Const x l
  reloc l (Lit x _) = Lit x l
  reloc l (App x y _) = App x y l
  reloc l (Lam x e _) = Lam x e l
  reloc l (Op x _) = Op x l
  reloc l (Hole _) = Hole l
  reloc l (Quant op xs r t _) = Quant op xs r t l
  reloc _ (Subst e s) = Subst e s



type Subst = Map Text Expr

instance ToJSON Expr

instance FromJSON Expr

wrapLam :: [Text] -> Expr -> Expr
wrapLam [] body = body
wrapLam (x : xs) body = Lam x (wrapLam xs body) NoLoc

--------------------------------------------------------------------------------

-- | Variables and stuff
data Name = Name Text Loc
  deriving (Eq, Show, Generic)

instance ToConcrete Name C.Name where
  toConcrete (Name a l) = C.Name a l
  
instance Located Name where
  locOf (Name _ l) = l

instance ToJSON Name

instance FromJSON Name

instance Ord Name where
  compare (Name a _) (Name b _) = compare a b

nameToText :: Name -> Text
nameToText (Name x _) = x

--------------------------------------------------------------------------------

-- | Constructors
unary :: Op -> Expr -> Expr
unary op x = App (Op op NoLoc) x NoLoc

binary :: Op -> Expr -> Expr -> Expr
binary op x y = App (App (Op op NoLoc) x NoLoc) y (x <--> y)

lt, gt, gte, lte, eqq, conj, disj, implies :: Expr -> Expr -> Expr
lt = binary LT
gt = binary GT
gte = binary GTE
lte = binary LTE
eqq = binary EQ
conj = binary Conj
disj = binary Disj
implies = binary Implies

neg :: Expr -> Expr
neg = unary Neg

true :: Expr
true = Lit (Bol True) NoLoc

false :: Expr
false = Lit (Bol False) NoLoc

conjunct :: [Expr] -> Expr
conjunct [] = true
conjunct xs = foldl1 conj xs

disjunct :: [Expr] -> Expr
disjunct [] = false
disjunct xs = foldl1 disj xs

imply :: Expr -> Expr -> Expr
imply p q = App (App (Op Implies NoLoc) p NoLoc) q NoLoc

predEq :: Expr -> Expr -> Bool
predEq = (==)

constant :: Text -> Expr
constant x = Const (Name x NoLoc) NoLoc

variable :: Text -> Expr
variable x = Var (Name x NoLoc) NoLoc

number :: Int -> Expr
number n = Lit (Num n) NoLoc

--------------------------------------------------------------------------------

-- | Instance of Located
instance Located Expr where
  locOf (Var _ l) = l
  locOf (Const _ l) = l
  locOf (Lit _ l) = l
  locOf (Op _ l) = l
  locOf (App _ _ l) = l
  locOf (Lam _ _ l) = l
  locOf (Hole l) = l
  locOf (Quant _ _ _ _ l) = l
  locOf (Subst _ _) = NoLoc
