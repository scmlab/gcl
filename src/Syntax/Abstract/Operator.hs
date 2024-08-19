module Syntax.Abstract.Operator where

import           Syntax.Abstract                ( Expr(..)
                                                , Lit(..)
                                                , Chain(..)
                                                , Type(..)
                                                , TBase(..)
                                                )
import           Syntax.Common
import           Data.Text                      ( Text )
import           Data.Loc                       ( Loc(..)
                                                , (<-->)
                                                , locOf
                                                )
import           Prelude                 hiding ( Ordering(..) )

-- | Constructors
unary :: ArithOp -> Expr -> Expr
unary op x = App (Op op) x (x <--> op)

arith :: ArithOp -> Expr -> Expr -> Expr
arith op x y = App (App (Op op) x (x <--> op)) y (x <--> y)

chain :: ChainOp -> Expr -> Expr -> Expr -- TODO: This might be wrong. Needs further investigation.
chain op x y = Chain (More (Pure x (x <--> op)) op y (x <--> y))

lt, gt, gte, lte, eqq, conj, disj, implies, add :: Expr -> Expr -> Expr
lt = (chain . LT) NoLoc
gt = (chain . GT) NoLoc
gte = (chain . GTEU) NoLoc
lte = (chain . LTEU) NoLoc
eqq = (chain . EQ) NoLoc
conj = (arith . ConjU) NoLoc
disj = (arith . DisjU) NoLoc
implies = (arith . ImpliesU) NoLoc
add = (arith . Add) NoLoc

neg :: Expr -> Expr
neg = (unary . NegU) NoLoc

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
imply p q = App (App ((Op . ImpliesU) NoLoc) p (locOf p)) q (locOf q)

predEq :: Expr -> Expr -> Bool
predEq = (==)

constant :: Text -> Expr
constant x = Const (Name x NoLoc) NoLoc

variable :: Text -> Expr
variable x = Var (Name x NoLoc) NoLoc

nameVar :: Name -> Expr
nameVar x = Var x NoLoc

number :: Int -> Expr
number n = Lit (Num n) NoLoc

exists :: [Name] -> Expr -> Expr -> Expr
exists xs ran term = Quant (Op (DisjU NoLoc)) xs ran term NoLoc

forAll :: [Name] -> Expr -> Expr -> Expr
forAll xs ran term = Quant (Op (ConjU NoLoc)) xs ran term NoLoc

pointsTo, sConj, sImp :: Expr -> Expr -> Expr
pointsTo = (arith . PointsTo) NoLoc
sConj = (arith . SConj) NoLoc
sImp = (arith . SImp) NoLoc

sconjunct :: [Expr] -> Expr
sconjunct [] = true
sconjunct xs = foldl1 sConj xs

-- | Frequently Used Types / Type Operators

tBool :: Type
tBool = TBase TBool NoLoc

tInt :: Type
tInt = TBase TInt NoLoc

tFunc :: Type -> Type -> Type
s `tFunc` t = TFunc s t NoLoc
