{-# LANGUAGE DeriveGeneric #-}

module Syntax.Abstract where

import Control.Monad (liftM2)

import Data.List ((\\))
import Data.Aeson
import Data.Text.Lazy (Text, pack)
import GHC.Generics (Generic)
import Prelude hiding (Ordering(..))

import Syntax.Concrete (Fixity(..))
import Type ()

type Index = Int

--------------------------------------------------------------------------------
-- | Expricates and Expressions

data Lit  = Num Int | Bol Bool | Chr Char
          deriving (Show, Eq, Generic)

data Expr = Var    Var
          | Const  Const
          | Lit    Lit
          | Op     Op      --- built-in operators
          | App    Expr   Expr
          | Quant  Expr [Var] Expr Expr
          | Hole   Index  [Subst]
          deriving (Show, Eq, Generic)

data Op = EQ | NEQ | LTE | GTE | LT | GT   -- binary relations
        | Implies | Conj | Disj | Neg  -- logic operators
        | Add | Sub | Mul | Div | Mod       -- arithmetics
     deriving (Show, Eq, Generic)


classify :: Op -> Fixity
classify Implies = InfixR 1
classify Disj = InfixL 2
classify Conj = InfixL 3
classify Neg = Prefix 4
classify EQ = Infix 5
classify NEQ = Infix 6
classify LTE = Infix 6
classify GTE = Infix 6
classify LT = Infix 6
classify GT = Infix 6
classify Add = InfixL 7
classify Sub = InfixL 7
classify Mul = InfixL 8
classify Div = InfixL 8
classify Mod = InfixL 9

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

tt, ff :: Expr
tt = Lit (Bol True)
ff = Lit (Bol False)

conjunct :: [Expr] -> Expr
conjunct []     = tt
conjunct [p]    = p
conjunct (p:ps) = p `conj` conjunct ps

disjunct :: [Expr] -> Expr
disjunct []     = ff
disjunct [p]    = p
disjunct (p:ps) = p `disj` disjunct ps

neg :: Expr -> Expr
neg x = App (Op Neg) x

instance ToJSON Op where
instance ToJSON Lit where
instance ToJSON Expr where

predEq :: Expr -> Expr -> Bool
predEq = (==)

--------------------------------------------------------------------------------
-- | Substitution

  -- SCM: the substituion is generally not too large and
  --      a list should be sufficient.

type Subst = [(Text,Expr)]

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
subst env (Var x)     = return $ maybe (Var x) id (lookup x env)
subst env (Const x)   = return $ maybe (Const x) id (lookup x env)
subst _   (Op op)     = return $ Op op
subst _   (Lit n)     = return $ Lit n
subst env (App e1 e2) = App <$> subst env e1 <*> subst env e2
subst env (Hole idx subs) = return $ Hole idx (env:subs)
subst env (Quant op xs range term) = do
   op' <- subst env op
   (xs', range', term') <- subLocal xs range term
   let env' = filter (not . (`elem` xs') . fst) env
   Quant op' xs' <$> subst env' range' <*> subst env' term'
  where subLocal :: Fresh m => [Var] -> Expr -> Expr -> m ([Var], Expr, Expr)
        subLocal []     r t = return ([],r,t)
        subLocal (i:is) r t
          | i `elem` fre = do j <- freshVar "dm"    -- "dummy" variable
                              r' <- subst [(i,Var j)] r
                              t' <- subst [(i,Var j)] t
                              first3 (j:) <$> subLocal is r' t'
          | otherwise = first3 (i:) <$> subLocal is r t
        fre = freeSubst env
        first3 f (x,y,z) = (f x, y, z)

free :: Expr -> [Var]
free (Var x)     = [x]
free (Const x)   = [x]
free (Op _)      = []
free (Lit _)     = []
free (App e1 e2) = free e1 ++ free e2  -- not worrying about duplication
free (Quant op xs range term) = (free op ++ free range ++ free term) \\ xs
free (Hole _ subs) = concat (map freeSubst subs) -- correct?

freeSubst :: Subst -> [Var]
freeSubst = concat . map (free . snd)

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
tInt  = TBase TInt
tBool = TBase TBool
tChar = TBase TChar

instance ToJSON Endpoint where
instance ToJSON Interval where
instance ToJSON TBase where
instance ToJSON Type where

---- obsolete. Kept temporarily


{-
data Program = Program
                [Declaration]               -- declarations
                (Maybe ([Stmt], Expr, Loc)) -- statements + postcondition
              deriving (Eq, Show)

data Declaration
  = ConstDecl [Const] Type
  | VarDecl [Var] Type
  deriving (Eq, Show)
-}
{-
data Stmt
  = Skip                          Loc
  | Abort                         Loc
  | Assign  [Var] [Expr]          Loc
  | Assert  Expr                  Loc
  | Do      Expr Expr [GdCmd]     Loc
  | If      (Maybe Expr) [GdCmd]  Loc
  | Spec                          Loc
  deriving (Eq, Show)


instance Located Stmt where
  locOf (Skip l)        = l
  locOf (Abort l)       = l
  locOf (Assign _ _ l)  = l
  locOf (Assert _ l)    = l
  locOf (Do _ _ _ l)    = l
  locOf (If _ _ l)      = l
  locOf (Spec l)        = l

data GdCmd = GdCmd Expr [Stmt] Loc deriving (Eq, Show)

getGuards :: [GdCmd] -> [Expr]
getGuards = fst . unzipGdCmds

unzipGdCmds :: [GdCmd] -> ([Expr], [[Stmt]])
unzipGdCmds = unzip . map (\(GdCmd x y _) -> (x, y))

--------------------------------------------------------------------------------
-- | Affixing assertions to DO or IF constructs.

infixr 3 <:>
(<:>) :: Monad m => m a -> m [a] -> m [a]
(<:>) = liftM2 (:)

affixAssertions :: [C.Stmt] -> AbstractM [Stmt]
-- affixAssertions = undefined
affixAssertions      []  = return []
affixAssertions (  x:[]) = (:) <$> fromConcrete x <*> pure []
affixAssertions (x:y:xs) = case (x, y) of
  -- AssertWithBnd + DO : affix!
  (C.AssertWithBnd p e _, C.Do q loc) ->
    Do  <$> fromConcrete p <*> fromConcrete e <*> mapM fromConcrete q <*> pure loc
        <:> affixAssertions xs

  -- AssertWithBnd + _
  (C.AssertWithBnd _ _ loc, _) -> throwError $ ExcessBound loc

  -- Assert + DO
  (C.Assert _ loc, C.Do _ _) -> throwError $ MissingBound loc

  -- Assert + If : affix!
  (C.Assert p _, C.If q loc) ->
    If  <$> fmap Just (fromConcrete p)
        <*> mapM fromConcrete q
        <*> pure loc
        <:> affixAssertions xs

  -- _ + Do
  (_, C.Do _ loc) -> throwError $ MissingAssertion loc

  -- otherwise
  _  -> fromConcrete x <:> affixAssertions (y:xs)
-}

--------------------------------------------------------------------------------
-- Converting from Concrete Syntax Tree

{-
instance FromConcrete C.Stmt Stmt where
  fromConcrete (C.Assert p   loc) = Assert  <$> fromConcrete p <*> pure loc
  fromConcrete (C.Skip       loc) = Skip    <$> pure loc
  fromConcrete (C.Abort      loc) = Abort   <$> pure loc
  fromConcrete (C.Assign p q loc) = Assign  <$> mapM fromConcrete p
                                            <*> mapM fromConcrete q
                                            <*> pure loc
  fromConcrete (C.If     p   loc) = If      <$> pure Nothing
                                            <*> mapM fromConcrete p
                                            <*> pure loc

  -- Panic because these cases should've been handled by `affixAssertions`
  fromConcrete (C.AssertWithBnd _ _ _) = throwError $ Panic "AssertWithBnd"
  fromConcrete (C.Do     _ _) = throwError $ Panic "Do"
  -- Holes and specs
  fromConcrete (C.SpecQM loc) = throwError $ DigHole loc
  fromConcrete (C.Spec loc) = Spec <$> pure loc

-- deals with missing Assertions and Bounds
instance FromConcrete [C.Stmt] [Stmt] where
  fromConcrete      []  = return []
  fromConcrete (x : []) = case x of
    C.Do _ loc -> throwError $ MissingAssertion loc
    _          -> fromConcrete x <:> pure []
  fromConcrete (x:y:xs) = affixAssertions (x:y:xs)

instance FromConcrete C.GdCmd GdCmd where
  fromConcrete (C.GdCmd p q l) = GdCmd  <$> fromConcrete p
                                        <*> fromConcrete q
                                        <*> pure l

instance FromConcrete C.Declaration Declaration where
  fromConcrete (C.ConstDecl p q _) = ConstDecl  <$> mapM fromConcrete p
                                                <*> fromConcrete q
  fromConcrete (C.VarDecl   p q _) = VarDecl    <$> mapM fromConcrete p
                                                <*> fromConcrete q

instance FromConcrete C.Program Program where
  fromConcrete (C.Program p q _) = Program  <$> mapM fromConcrete p
                                            <*> (fromConcrete q >>= checkStatements)
    where
      -- check if the postcondition of the whole program is missing
      checkStatements :: [Stmt] -> AbstractM (Maybe ([Stmt], Expr, Loc))
      checkStatements [] = return Nothing
      checkStatements xs = case last xs of
        Assert r l -> return (Just (init xs, r, l))
        _          -> throwError MissingPostcondition


--------------------------------------------------------------------------------
-- | Convert Error

data ConvertError
  = MissingAssertion Loc
  | MissingBound     Loc
  | ExcessBound      Loc
  | MissingPostcondition
  | DigHole Loc
  | Panic String
  deriving (Eq, Show, Generic)

instance Located ConvertError where
  locOf (MissingAssertion loc) = loc
  locOf (MissingBound loc) = loc
  locOf (ExcessBound loc) = loc
  locOf MissingPostcondition = NoLoc
  locOf (DigHole loc) = loc
  locOf (Panic _) = NoLoc

instance ToJSON ConvertError where
-}
