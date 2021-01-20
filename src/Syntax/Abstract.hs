{-# LANGUAGE DeriveGeneric #-}

module Syntax.Abstract where

import           Control.Monad                  ( liftM2 )

import           Data.List                      ( (\\) )
import           Data.Map (Map)
import qualified Data.Map as Map

import           Data.Aeson
import           Data.Text.Lazy                 ( Text
                                                , pack
                                                )
import           GHC.Generics                   ( Generic )
import           Prelude                 hiding ( Ordering(..) )

import Data.Maybe (fromMaybe)
import Data.Loc 
import Syntax.Common 

--------------------------------------------------------------------------------
-- | Expressions
type Index = Int
data Lit  = Num Int | Bol Bool | Chr Char
          deriving (Show, Eq, Generic)

instance Located Lit where
  locOf _ = NoLoc

instance FromJSON Lit

data Expr = Var    Var
          | Const  Const
          | Lit    Lit
          | Op     Op      --- built-in operators
          | App    Expr   Expr
          | Quant  Expr   [Var] Expr Expr -- (+ i : 0 <= i && i < N : f i)
          | Hole   Index  [Subst]
          | Subst  Expr    Subst
          deriving (Show, Eq, Generic)

type Subst = Map Text Expr

-- convenient constructors
lt, gt, gte, lte, eqq, conj, disj, implies :: Expr -> Expr -> Expr
x `lt` y = App (App (Op LT) x) y
x `gt` y = App (App (Op GT) x) y
x `gte` y = App (App (Op GTE) x) y
x `lte` y = App (App (Op LTE) x) y
x `eqq` y = App (App (Op EQ) x) y
x `conj` y = App (App (Op Conj) x) y
x `disj` y = App (App (Op Disj) x) y
x `implies` y = App (App (Op Implies) x) y

plus, minus, mul, divi :: Expr -> Expr -> Expr
x `plus` y = App (App (Op Add) x) y
x `minus` y = App (App (Op Sub) x) y
x `mul` y = App (App (Op Mul) x) y
x `divi` y = App (App (Op Div) x) y  -- to avoid name clash

var :: String -> Expr
var = Var . pack

litN :: Int -> Expr
litN = Lit . Num

litB :: Bool -> Expr
litB = Lit . Bol

litC :: Char -> Expr
litC = Lit . Chr

true, false :: Expr
true = Lit (Bol True)
false = Lit (Bol False)

conjunct :: [Expr] -> Expr
conjunct = foldl conj true

disjunct :: [Expr] -> Expr
disjunct = foldl disj true

imply :: Expr -> Expr -> Expr
imply p q = App (App (Op Implies) p) q

neg :: Expr -> Expr
neg x = App (Op Neg) x

instance ToJSON Lit where
instance ToJSON Expr where

predEq :: Expr -> Expr -> Bool
predEq = (==)

--------------------------------------------------------------------------------
-- | Substitution

  -- SCM: substituion needs fresh names. However, I don't
  --      want to move M into this module. Therefore I am
  --      using a type class.

class Monad m => Fresh m where
  fresh :: m Int
  freshVar :: String -> m Var
  freshVars :: String -> Int -> m [Var]

  freshVar prefix =
    (\i -> pack ("_" ++ prefix ++ show i)) <$> fresh

  freshVars _  0 = return []
  freshVars pf n = liftM2 (:) (freshVar pf) (freshVars pf (n-1))

subst :: Fresh m => Subst -> Expr -> m Expr
subst env (Var   x               ) = return $ fromMaybe (Var x) (Map.lookup x env)
subst env (Const x               ) = return $ fromMaybe (Const x) (Map.lookup x env)
subst _   (Op    op              ) = return $ Op op
subst _   (Lit   n               ) = return $ Lit n
subst env (App  e1  e2           ) = App <$> subst env e1 <*> subst env e2
subst env (Hole idx subs         ) = return $ Hole idx (env : subs)
subst env (Quant op xs range term) = do
  op'                  <- subst env op
  (xs', range', term') <- subLocal xs range term
  let env' = Map.filterWithKey (\key expr -> key `notElem` xs') env
  Quant op' xs' <$> subst env' range' <*> subst env' term'
 where
  subLocal :: Fresh m => [Var] -> Expr -> Expr -> m ([Var], Expr, Expr)
  subLocal [] r t = return ([], r, t)
  subLocal (i : is) r t
    | i `elem` fre = do
      j  <- freshVar "dm"    -- "dummy" variable
      r' <- subst (Map.singleton i (Var j)) r
      t' <- subst (Map.singleton i (Var j)) t
      first3 (j :) <$> subLocal is r' t'
    | otherwise = first3 (i :) <$> subLocal is r t
  fre = freeSubst env
  first3 f (x, y, z) = (f x, y, z)
subst env (Subst _ _) = error "subst on Subst not defined"

free :: Expr -> [Var]
free (Var   x               ) = [x]
free (Const x               ) = [x]
free (Op    _               ) = []
free (Lit   _               ) = []
free (App e1 e2             ) = free e1 ++ free e2  -- not worrying about duplication
free (Quant op xs range term) = (free op ++ free range ++ free term) \\ xs
free (Hole _ subs           ) = subs >>= freeSubst -- correct?
free (Subst _ _           )   = error "free on Subst not defined"

freeSubst :: Subst -> [Var]
freeSubst env = Map.elems env >>= free 

--------------------------------------------------------------------------------
-- | Variables

type Const = Text
type Var = Text
type TVar = Text

--------------------------------------------------------------------------------
-- | Types

data Endpoint = Including Expr | Excluding Expr deriving (Show, Eq, Generic)
data Interval = Interval Endpoint Endpoint deriving (Show, Eq, Generic)

data TBase = TInt | TBool | TChar
      deriving (Show, Eq, Generic)

data Type = TBase TBase
          | TArray Interval Type
          | TFunc Type Type
          | TVar TVar
      deriving (Show, Eq, Generic)

tInt, tBool, tChar :: Type
tInt = TBase TInt
tBool = TBase TBool
tChar = TBase TChar

instance ToJSON Endpoint where
instance ToJSON Interval where
instance ToJSON TBase where
instance ToJSON Type where

--------------------------------------------------------------------------------
-- | Fixity & Precedence

data Fixity = Infix Int | InfixR Int | InfixL Int | Prefix Int | Postfix Int
  deriving (Show, Eq)

classify :: Op -> Fixity
classify Implies = InfixR 1
classify Disj    = InfixL 2
classify Conj    = InfixL 3
classify Neg     = Prefix 4
classify EQ      = Infix 5
classify NEQ     = Infix 6
classify LTE     = Infix 6
classify GTE     = Infix 6
classify LT      = Infix 6
classify GT      = Infix 6
classify Add     = InfixL 7
classify Sub     = InfixL 7
classify Mul     = InfixL 8
classify Div     = InfixL 8
classify Mod     = InfixL 9
classify Sum     = Prefix 5
classify Exists  = Prefix 6
classify Forall  = Prefix 7
