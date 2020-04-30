{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

module Syntax.Concrete
  ( module Syntax.Concrete
  , Op(..)
  , TBase(..)
  , Lit(..)  -- re-exporting from Syntax.Abstract
  )
where

import           Data.Aeson
import           Data.Loc
import           Data.Map                       ( Map )
import qualified Data.Map                      as Map
import           Data.Set                       ( Set
                                                , (\\)
                                                )
import qualified Data.Set                      as Set
import           Data.Text.Lazy                 ( Text )
import qualified Data.Text.Lazy                as Text
import           Prelude                 hiding ( Ordering(..) )
import           GHC.Generics                   ( Generic )

import           Syntax.Abstract                ( Op(..)
                                                , TBase(..)
                                                , Lit(..)
                                                )

--------------------------------------------------------------------------------
-- | Program / Declaration / Statement

data Program = Program
      [Declaration]            -- constant and variable declarations
      [Expr]                   -- global properties
      [(Upper, [Lower], Expr)] -- let bindings
      [Stmt]                   -- main program
      Loc
  deriving (Eq, Show)

data Declaration
  = ConstDecl [Upper] Type (Maybe Expr) Loc
  | VarDecl [Lower] Type (Maybe Expr) Loc
  | LetDecl Upper [Lower] Expr Loc
  deriving (Eq, Show)

data Stmt
  = Skip                      Loc
  | Abort                     Loc
  | Assign  [Lower] [Expr]    Loc
  | Assert  Expr              Loc
  | LoopInvariant  Expr Expr  Loc
  | Do            [GdCmd]     Loc
  | If            [GdCmd]     Loc
  | SpecQM                    Loc -- ? to be rewritten as {!!} by the frontend
  | Spec                      Loc
  deriving (Eq, Show)

data GdCmd = GdCmd Expr [Stmt] Loc deriving (Eq, Show)

extractAssertion :: Declaration -> Maybe Expr
extractAssertion (ConstDecl _ _ e _) = e
extractAssertion (VarDecl   _ _ e _) = e
extractAssertion (LetDecl _ _ _ _  ) = Nothing

extractLetBinding :: Declaration -> Maybe (Upper, [Lower], Expr)
extractLetBinding (ConstDecl _ _ _ _) = Nothing
extractLetBinding (VarDecl   _ _ _ _) = Nothing
extractLetBinding (LetDecl c a e _  ) = Just (c, a, e)

getGuards :: [GdCmd] -> [Expr]
getGuards = fst . unzipGdCmds

unzipGdCmds :: [GdCmd] -> ([Expr], [[Stmt]])
unzipGdCmds = unzip . map (\(GdCmd x y _) -> (x, y))

--------------------------------------------------------------------------------
-- | Types

data Endpoint = Including Expr | Excluding Expr deriving (Eq, Show)
data Interval = Interval Endpoint Endpoint Loc deriving (Eq, Show)

data Type = TBase TBase Loc
          | TArray Interval Type Loc
          | TFunc Type Type Loc
          | TVar Lower Loc
          deriving (Eq, Show)

--------------------------------------------------------------------------------
-- | Expressions

data Expr = Lit   Lit       Loc
          | Var   Lower     Loc
          | Const Upper     Loc
          | Op    Op        Loc
          | App   Expr Expr Loc
          | Hole            Loc
          | Quant Expr [Lower] Expr Expr Loc
          deriving (Eq, Show, Generic)

instance ToJSON Expr where

--------------------------------------------------------------------------------
-- | Variables and stuff

data Upper = Upper Text Loc
  deriving (Eq, Show, Generic)

instance ToJSON Upper where
instance Ord Upper where
  compare (Upper a _) (Upper b _) = compare a b

data Lower = Lower Text Loc
  deriving (Eq, Show, Generic)

instance ToJSON Lower where
instance Ord Lower where
  compare (Lower a _) (Lower b _) = compare a b

upperToText :: Upper -> Text
upperToText (Upper x _) = x

lowerToText :: Lower -> Text
lowerToText (Lower x _) = x

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
constant x = Const (Upper x NoLoc) NoLoc

variable :: Text -> Expr
variable x = Var (Lower x NoLoc) NoLoc

number :: Int -> Expr
number n = Lit (Num n) NoLoc

--------------------------------------------------------------------------------
-- | Instance of Located

instance Located Expr where
  locOf (Var   _ l      ) = l
  locOf (Const _ l      ) = l
  locOf (Lit   _ l      ) = l
  locOf (Op    _ l      ) = l
  locOf (App _ _ l      ) = l
  locOf (Hole l         ) = l
  locOf (Quant _ _ _ _ l) = l

--------------------------------------------------------------------------------
-- | Substitution

-- Names are "read-only", they can be variables or constants
type Name = Either Lower Upper
type Subst = Map Name Expr

-- Vars can be substituted. They can only be variables
type Var = Lower


  -- SCM: substituion needs fresh names. However, I don't
  --      want to move M into this module. Therefore I am
  --      using a type class.

class Monad m => Fresh m where
  fresh :: m Int

  -- generate a fresh var with a given prefix
  freshVar :: Text -> m Var
  freshVar prefix = do
    i <- fresh
    let name = "_" <> prefix <> Text.pack (show i)
    return $ Lower name NoLoc

  -- generate a bunch of fresh vars with a given prefix
  freshVars :: Text -> Int -> m [Var]
  freshVars _  0 = return []
  freshVars pf n = (:) <$> freshVar pf <*> freshVars pf (n-1)

subst :: Fresh m => Subst -> Expr -> m Expr
subst env (Var x l) = return $ maybe (Var x l) id (Map.lookup (Left x) env)
subst env (Const x l) =
  return $ maybe (Const x l) id (Map.lookup (Right x) env)
subst _   (Op  op l                ) = return $ Op op l
subst _   (Lit n  l                ) = return $ Lit n l
subst env (App e1 e2 l) = App <$> subst env e1 <*> subst env e2 <*> pure l
subst _   (Hole l                  ) = return $ Hole l
-- subst env (Hole      l) = return $ Hole 0 [env]
-- subst env (Hole idx subs l) = return $ Hole idx (env:subs)
subst env (Quant op xs range term l) = do
  op'                  <- subst env op
  (xs', range', term') <- subLocal xs range term

  let env' = Map.filterWithKey
        (\k _ -> case k of
          Left  k' -> not (k' `elem` xs')
          Right _  -> False
        )
        env

  Quant op' xs' <$> subst env' range' <*> subst env' term' <*> pure l

 where
  subLocal :: Fresh m => [Var] -> Expr -> Expr -> m ([Var], Expr, Expr)
  subLocal [] r t = return ([], r, t)
  subLocal (i : is) r t
    | Left i `elem` freeInEnv = do
        -- if `i` is a free variable of `env`
        -- instantiate a dummy varialbe `j`
      j  <- freshVar "dummy"
      -- substitute the variable `i` in `r` with the dummy variable
      r' <- subst (Map.singleton (Left i) (Var j NoLoc)) r
      -- substitute the variable `i` in `t` with the dummy variable
      t' <- subst (Map.singleton (Left i) (Var j NoLoc)) t
      first3 (j :) <$> subLocal is r' t'
    | otherwise = first3 (i :) <$> subLocal is r t

  freeInEnv = freeSubst env
  first3 f (x, y, z) = (f x, y, z)

free :: Expr -> Set Name
free (Var   x _  ) = Set.singleton (Left x)
free (Const x _  ) = Set.singleton (Right x)
free (Op    _ _  ) = mempty
free (Lit   _ _  ) = mempty
free (App e1 e2 _) = free e1 <> free e2
free (Quant op xs range term _) =
  (free op <> free range <> free term) \\ Set.fromList (map Left xs)
free (Hole _) = mempty -- banacorn: `subs` has been always empty anyway
    -- concat (map freeSubst subs) -- correct?

-- free variables in the Subst table
freeSubst :: Subst -> Set Name
freeSubst = Set.unions . map free . Map.elems
