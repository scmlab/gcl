module Syntax.Abstract.Located where

import Data.Loc
import Data.Text (Text)
import Syntax.Common.Located()
import Syntax.Abstract
import Syntax.Common
import Prelude hiding (Ordering(..))

instance Located Program where
  locOf (Program _ _ _ _ l) = l

instance Located Stmt where
  locOf (Skip l) = l
  locOf (Abort l) = l
  locOf (Assign _ _ l) = l
  locOf (Assert _ l) = l
  locOf (LoopInvariant _ _ l) = l
  locOf (Do _ l) = l
  locOf (If _ l) = l
  locOf (Spec _ l) = l
  locOf (Proof l) = l

instance Located Endpoint where
  locOf (Including e) = locOf e
  locOf (Excluding e) = locOf e

instance Located Interval where
  locOf (Interval _ _ l) = l

instance Located Type where
  locOf (TBase _ l) = l
  locOf (TArray _ _ l) = l
  locOf (TFunc _ _ l) = l
  locOf (TVar _ l) = l

instance Located Expr where
  locOf (Var _ l) = l
  locOf (Const _ l) = l
  locOf (Lit _ l) = l
  locOf (Op op) = locOf op
  locOf (Chain _ _ _ l) = l
  locOf (App _ _ l) = l
  locOf (Lam _ _ l) = l
  locOf (Hole l) = l
  locOf (Quant _ _ _ _ l) = l
  locOf (Subst _ _ x) = locOf x

instance Located Lit where
  locOf _ = NoLoc
  
-- | Constructors
unary :: ArithOp -> Expr -> Expr
unary op x = App (Op op) x (x <--> op)

binary :: ArithOp -> Expr -> Expr -> Expr
binary op x y = App (App (Op op) x (x <--> op)) y (x <--> y)

chain :: ChainOp -> Expr -> Expr -> Expr
chain op x y = Chain x op y (x <--> y)

lt, gt, gte, lte, eqq, conj, disj, implies :: Expr -> Expr -> Expr
lt = (chain . LT) NoLoc
gt = (chain . GT) NoLoc
gte = (chain . GTEU) NoLoc
lte = (chain . LTEU) NoLoc
eqq = (chain . EQ) NoLoc
conj = (binary . ConjU) NoLoc
disj = (binary . DisjU) NoLoc
implies = (binary . ImpliesU) NoLoc

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

number :: Int -> Expr
number n = Lit (Num n) NoLoc