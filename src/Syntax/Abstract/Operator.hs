module Syntax.Abstract.Operator where

import Syntax.Abstract (Expr (..),Lit (..))
import Syntax.Abstract.Located()
import Syntax.Common
import Data.Text (Text)
import Data.Loc (Loc(..), (<-->), locOf)
import Prelude hiding (Ordering(..))

-- | Constructors
unary :: ArithOp -> Expr -> Expr
unary op x = App (Op (ArithOp op)) x (x <--> op)

binary :: ArithOp -> Expr -> Expr -> Expr
binary op x y = App (App (Op (ArithOp op)) x (x <--> op)) y (x <--> y)

chain :: ChainOp -> Expr -> Expr -> Expr
chain op x y = Chain x (ChainOp op) y (x <--> y)

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
imply p q = App (App ((Op . ArithOp . ImpliesU) NoLoc) p (locOf p)) q (locOf q)

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
exists xs ran term =
  Quant (Op (QuantOp (Exists NoLoc))) xs ran term NoLoc
