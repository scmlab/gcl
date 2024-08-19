module Syntax.Typed.Operator where

import           Syntax.Abstract.Operator ( tBool, tInt, tFunc )
import           Syntax.Typed.Types
import           Syntax.Typed.Util        ( typeOf )
import           Syntax.Typed.Instances.Located
import           Syntax.Common
import           Data.Text                      ( Text )
import           Data.Loc                       ( Loc(..)
                                                , (<-->)
                                                , locOf
                                                )
import           Prelude                 hiding ( Ordering(..) )
import           Syntax.Abstract.Types          ( Type(..), Lit (..) )

unary :: ArithOp -> Type -> Expr -> Expr
unary op t x = App (Op (ArithOp op) t) x (x <--> op)

arith :: ArithOp -> Type -> Expr -> Expr -> Expr
arith op t x y = App (App (Op (ArithOp op) t) x (x <--> op)) y (x <--> y)

chain :: ChainOp -> Type -> Expr -> Expr -> Expr -- TODO: This might be wrong. Needs further investigation.
chain op t x y = Chain (More (Pure x) (ChainOp op) t y)

neg :: Expr -> Expr
neg = unary (NegU NoLoc) (tBool `tFunc` tBool)

  -- Type of binary logic operators: Bool -> Bool -> Bool
  -- Type of binary Int operators: Int -> Int -> Int, etc.
  -- Because they are used in many occassions.

tBinLogicOp :: Type
tBinLogicOp = tBool `tFunc` (tBool `tFunc` tBool)

tBinIntOp :: Type
tBinIntOp = tInt `tFunc` (tInt `tFunc` tInt)

tBinIntROp :: Type
tBinIntROp = tInt `tFunc` (tInt `tFunc` tBool)

lt, gt, gte, lte :: Expr -> Expr -> Expr
lt  = chain (LT   NoLoc) tBinIntROp
gt  = chain (GT   NoLoc) tBinIntROp
gte = chain (GTEU NoLoc) tBinIntROp
lte = chain (LTEU NoLoc) tBinIntROp

conj, disj, implies :: Expr -> Expr -> Expr
conj    = arith (ConjU    NoLoc) tBinLogicOp
disj    = arith (DisjU    NoLoc) tBinLogicOp
implies = arith (ImpliesU NoLoc) tBinLogicOp

add :: Expr -> Expr -> Expr
add    = arith (Add    NoLoc) tBinIntOp

eqq :: Expr -> Expr -> Expr
e0 `eqq` e1 =
   Chain (More (Pure e0) (ChainOp (EQ NoLoc)) (typeOf e0) e1)

true, false :: Expr
true  = Lit (Bol True)  tBool NoLoc
false = Lit (Bol False) tBool NoLoc

conjunct :: [Expr] -> Expr
conjunct [] = true
conjunct xs = foldl1 conj xs

disjunct :: [Expr] -> Expr
disjunct [] = false
disjunct xs = foldl1 disj xs

predEq :: Expr -> Expr -> Bool
predEq = (==)

constant :: Text -> Type -> Expr
constant x t = Const (Name x NoLoc) t NoLoc

variable :: Text -> Type -> Expr
variable x t = Var (Name x NoLoc) t NoLoc

nameVar :: Name -> Type -> Expr
nameVar x t = Var x t NoLoc

number :: Int -> Expr
number n = Lit (Num n) tInt NoLoc

exists :: [Name] -> Expr -> Expr -> Expr
exists xs ran term = Quant (Op (ArithOp (DisjU NoLoc)) tBinLogicOp)
                           xs ran term NoLoc

forAll :: [Name] -> Expr -> Expr -> Expr
forAll xs ran term = Quant (Op (ArithOp (ConjU NoLoc)) tBinLogicOp)
                           xs ran term NoLoc

pointsTo, sConj, sImp :: Expr -> Expr -> Expr
pointsTo = arith (PointsTo NoLoc) tBinIntOp
sConj    = arith (SConj    NoLoc) tBinLogicOp
sImp     = arith (SImp     NoLoc) tBinLogicOp

sconjunct :: [Expr] -> Expr
sconjunct [] = true
sconjunct xs = foldl1 sConj xs
