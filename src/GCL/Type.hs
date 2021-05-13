{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module GCL.Type where

import Control.Monad.Except
import Control.Monad.RWS hiding (Sum)
import Data.Aeson (ToJSON)
import Data.Loc
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Set (Set)
import qualified Data.Set as Set
import qualified Data.Text as Text
import GHC.Generics (Generic)
import Syntax.Abstract
import Syntax.Abstract.Located ()
import Syntax.Common
import Prelude hiding (Ordering (..))

type TM = Except TypeError

------------------------------------------
-- type enviornment
------------------------------------------

-- newtype TypeEnv = TypeEnv (Map Name Type) deriving (Eq, Show)
type TypeEnv = Map Name Type

emptyEnv :: TypeEnv
emptyEnv = Map.empty

extend :: TypeEnv -> (Name, Type) -> TypeEnv
extend env (x, s) =  Map.insert x s env

extend' :: TypeEnv -> (Name, Type) -> TM TypeEnv
extend' e@env (x, s) =
  case Map.lookup x env of
    Nothing -> return $ e `extend` (x, s)
    Just t -> throwError $ RecursiveType x t (locOf t)

------------------------------------------
-- Substitution
------------------------------------------

type SubstT = Map Name Type

emptySubstT :: SubstT
emptySubstT = Map.empty

compose :: SubstT -> SubstT -> SubstT
s1 `compose` s2 = s1 `Map.union` Map.map (apply s1) s2

class Substitutable a where
  apply :: SubstT -> a -> a
  fv :: a -> Set Name

instance Substitutable Type where
  apply _ t@(TBase _ _) = t
  apply s (TArray i t l) = TArray i (apply s t) l
  apply s (TFunc t1 t2 l) = TFunc (apply s t1) (apply s t2) l
  apply s t@(TVar x _) = Map.findWithDefault t x s

  fv (TBase _ _) = Set.empty
  fv (TArray _ t _) = fv t
  fv (TFunc t1 t2 _) = fv t1 `Set.union` fv t2
  fv (TVar x _) = Set.singleton x

-- instance Substitutable TypeEnv where
--   apply s env =  Map.map (apply s) env

--   fv env = foldl (flip (Set.union . fv)) Set.empty (Map.elems env)

occursT :: (Substitutable a) => Name -> a -> Bool
occursT x s = x `Set.member` fv s

typeWithLoc :: Loc -> Type -> Type
typeWithLoc l (TBase b _) = TBase b l
typeWithLoc l (TArray i t _) = TArray i t l
typeWithLoc l (TFunc t1 t2 _) = TFunc t1 t2 l
typeWithLoc l (TVar n _) = TVar n l

------------------------------------------
-- type inference
------------------------------------------

type Infer = RWST TypeEnv [Constraint] InferState TM

type InferState = Int

data TypeError
  = NotInScope Name Loc
  | UnifyFailed Type Type Loc
  | RecursiveType Name Type Loc
  | NotFunction Type Loc
  deriving (Show, Eq, Generic)

instance ToJSON TypeError

instance Located TypeError where
  locOf (NotInScope _ l) = l
  locOf (UnifyFailed _ _ l) = l
  locOf (RecursiveType _ _ l) = l
  locOf (NotFunction _ l) = l

initInfer :: InferState
initInfer = 0

runInfer' :: TypeEnv -> Infer Type -> Either TypeError (Type, [Constraint])
runInfer' env m = runExcept $ evalRWST m env initInfer

runInfer :: TypeEnv -> Infer Type -> TM (Type, [Constraint])
runInfer env m = evalRWST m env initInfer

infer :: Expr -> Infer Type
infer (Lit lit l) = return (litTypes lit l)
infer (Var x _) = lookupEnv x
infer (Const c _) = lookupEnv c
infer (Op o) = return (arithOpTypes o)
infer (Chain a op b loc) = do
  ta <- infer a
  top <- inferChainOpTypes op
  tb <- infer b

  case (a, b) of
    (Chain _ _ l _, Chain r _ _ _) -> do
      tl <- infer l
      tr <- infer r
      -- check type of `l op r` is bool
      uni top (TFunc tl (TFunc tr (TBase TBool (locOf op)) (locOf r)) (l <--> r))
    (Chain _ _ l _, _) -> do
      tl <- infer l
      uni top (TFunc tl (TFunc tb (TBase TBool (locOf op)) (locOf b)) (l <--> b))
    (_, Chain r _ _ _) -> do
      tr <- infer r
      uni top (TFunc ta (TFunc tr (TBase TBool (locOf op)) (locOf r)) (a <--> r))
    (_, _) -> do
      uni top (TFunc ta (TFunc tb (TBase TBool (locOf op)) (locOf b)) (a <--> b))
  return (TBase TBool loc)
infer (App e1 e2 l) = do
  t1 <- infer e1
  t2 <- infer e2
  v <- fresh l
  uni t1 (TFunc t2 v l)
  return v
infer (Lam x e l) = do
  v <- fresh l
  t <- inEnv [(x, v)] (infer e)
  return (TFunc v t l)
infer (Hole l) = fresh l
infer (Quant qop iters rng t l) = do
  tr <- inEnv [(n, TBase TInt (locOf n)) | n <- iters] (infer rng)
  uni tr (TBase TBool (locOf rng))

  tt <- inEnv [(n, TBase TInt (locOf n)) | n <- iters] (infer t)
  case qop of
    Left (QuantOp (Hash _)) -> do
      uni tt (TBase TBool (locOf t))
      return (TBase TInt l)
    Left op -> do
      let to = opTypes op
      f tt to
    Right qop' -> do
      to <- infer qop'
      f tt to
    where
      f tt to = do
        x <- fresh l
        uni to (TFunc x (TFunc x x (locOf qop)) (locOf qop))
        uni tt x
        return x
infer (Subst expr sub) = do
  t <- infer expr
  s <- mapM infer sub
  return $ apply s t

inferExpr :: TypeEnv -> Expr -> TM Type
inferExpr env e = do
  (t, cs) <- runInfer env (infer e)
  s <- runSolver cs
  return $ apply s t

inferDecl :: TypeEnv -> Declaration -> TM TypeEnv
inferDecl env (ConstDecl ns t p _) = do
  checkType env t
  env' <- foldM f env ns
  forM_ p (checkPredicate env')
  return env'
  where
    f env' n = env' `extend'` (n, t)
inferDecl env (VarDecl ns t p _) = do
  checkType env t
  env' <- foldM f env ns
  forM_ p (checkPredicate env')
  return env'
  where
    f env' n = env' `extend'` (n, t)
inferDecl env (LetDecl n args expr _) = do
  s <- inferExpr env expr'
  env `extend'` (n, s)
  where
    expr' = foldr (\a e' -> Lam a e' (locOf e')) expr args

uni :: Type -> Type -> Infer ()
uni t1 t2 = tell [(t1, t2)]

lookupEnv :: Name -> Infer Type
lookupEnv n = do
  env <- ask
  case Map.lookup n env of
    Just t -> return (typeWithLoc (locOf n) t)
    Nothing -> throwError $ NotInScope n (locOf n)

inEnv :: [(Name, Type)] -> Infer a -> Infer a
inEnv l m = do
  let scope e = foldl (\e' (x, sc) -> Map.insert x sc e') e l
  local scope m

fresh :: Loc -> Infer Type
fresh l = flip TVar l . flip Name l <$> freshText
  where
    freshText = do
      i <- get
      put (i + 1)
      return . Text.pack $ ("?m_" ++ show i)

------------------------------------------
-- type check
------------------------------------------

checkName :: TypeEnv -> (Name, Expr) -> TM ()
checkName env@envM (n, expr) =
  case Map.lookup n envM of
    Nothing -> throwError $ NotInScope n (locOf n)
    Just t -> do
      checkIsType env expr t

checkIsType :: TypeEnv -> Expr -> Type -> TM ()
checkIsType env expr t = do
  (eType, cs) <- runInfer env (infer expr)
  void $ runSolver (cs `mappend` [(eType, t)])

checkPredicate :: TypeEnv -> Expr -> TM ()
checkPredicate env p =
  checkIsType env p (TBase TBool NoLoc)

checkType :: TypeEnv -> Type -> TM ()
checkType _ (TBase _ _) = return ()
checkType env (TArray (Interval e1 e2 _) t _) = do
  checkIsType env (getEndpointExpr e1) (TBase TInt NoLoc)
  checkIsType env (getEndpointExpr e2) (TBase TInt NoLoc)
  checkType env t
  where
    getEndpointExpr (Including e) = e
    getEndpointExpr (Excluding e) = e
checkType env (TFunc t1 t2 _) = checkType env t1 >> checkType env t2
checkType _ (TVar _ _) = return ()

checkExpr :: TypeEnv -> Expr -> TM ()
checkExpr env expr = void $ inferExpr env expr

checkGdCmd :: TypeEnv -> GdCmd -> TM ()
checkGdCmd env (GdCmd expr stmts _) = do
  checkPredicate env expr
  mapM_ (checkStmt env) stmts

checkStmt :: TypeEnv -> Stmt -> TM ()
checkStmt _ (Skip _) = return ()
checkStmt _ (Abort _) = return ()
checkStmt env (Assign ns es _)            -- NOTE : Not sure if Assign work this way
  | length ns > length es = throwError $ error "Missing Expression"
  | length ns < length es = throwError $ error "Duplicated Assignment"
  | otherwise = forM_ (zip ns es) (checkName env)
checkStmt env (Assert expr _) = do
  checkPredicate env expr
checkStmt env (LoopInvariant e1 e2 _) = do
  checkPredicate env e1
  checkIsType env e2 (TBase TInt NoLoc)
checkStmt env (Do gdcmds _) = mapM_ (checkGdCmd env) gdcmds
checkStmt env (If gdcmds _) = mapM_ (checkGdCmd env) gdcmds
checkStmt _ (Spec _ _) = return ()
checkStmt _ (Proof _) = return ()

declsMap :: [Declaration] -> Map Name (Either Type Expr)
declsMap [] = Map.empty
declsMap (ConstDecl ns t _ _ : decls) =
  foldl (\m n -> Map.insert n (Left t) m) (declsMap decls) ns
declsMap (VarDecl ns t _ _ : decls) =
  foldl (\m n -> Map.insert n (Left t) m) (declsMap decls) ns
declsMap (LetDecl n xs body _ : decls) =
  Map.insert n (Right expr') (declsMap decls)
  where
    expr' = foldr (\x b -> Lam x b (b <--> locOf x)) body xs

checkProg :: Program -> TM ()
checkProg (Program decls exprs defs stmts _) = do
  env <- foldM inferDecl emptyEnv decls
  mapM_ (checkExpr env) exprs
  mapM_ (checkName env . (\(Defn name expr) -> (name, expr))) (Map.elems defs)
  mapM_ (checkStmt env) stmts

------------------------------------------
-- unification
------------------------------------------

type Constraint = (Type, Type)

type Unifier = (SubstT, [Constraint])

emptyUnifier :: Unifier
emptyUnifier = (emptySubstT, [])

unifies :: Type -> Type -> TM SubstT
unifies (TBase t1 _) (TBase t2 _)
  | t1 == t2 = return emptySubstT
unifies (TArray i1 t1 _) (TArray i2 t2 _)
  | i1 == i2 = unifies t1 t2
-- view array of type `t` as function type of `Int -> t`
unifies (TArray _ t1 _) (TFunc (TBase TInt _) t2 _) =
  unifies t1 t2
unifies (TFunc t1 t2 _) (TFunc t3 t4 _) = do
  s1 <- unifies t1 t3
  s2 <- unifies (apply s1 t2) (apply s1 t4)
  return (s2 `compose` s1)
unifies (TVar x l) t = bind x t l
unifies t (TVar x _) = bind x t (locOf t)
unifies t1 t2 = throwError $ UnifyFailed t1 t2 (locOf t1)

runSolver' :: [Constraint] -> Either TypeError SubstT
runSolver' cs = runExcept . solver $ (emptySubstT, cs)

runSolver :: [Constraint] -> TM SubstT
runSolver cs = solver (emptySubstT, cs)

solver :: Unifier -> TM SubstT
solver (s, []) = return s
solver (s, (t1, t2) : cs) = do
  su <- unifies t1 t2
  solver (su `compose` s, map (f (apply su)) cs)
  where
    f g (a, b) = (g a, g b)

bind :: Name -> Type -> Loc -> TM SubstT
bind x (TVar y _) _
  | x == y = return emptySubstT
bind x t l
  | occursT x t = throwError $ RecursiveType x t l
  | otherwise = return (Map.singleton x (typeWithLoc l t))

------------------------------------------
-- helper functions
------------------------------------------

litTypes :: Lit -> Loc -> Type
litTypes (Num _) l = TBase TInt l
litTypes (Bol _) l = TBase TBool l
litTypes (Chr _) l = TBase TChar l

-- NOTE : EQ, NEQ, NEQU is redundant here
inferChainOpTypes :: ChainOp -> Infer Type
inferChainOpTypes op = do
  case op of
    (EQ l) -> f l
    (NEQ l) -> f l
    (NEQU l) -> f l
    _ -> return (chainOpTypes op)
  where
    f l = do
      x <- fresh l
      return (TFunc x (TFunc x (TBase TBool l) l) l)


chainOpTypes :: ChainOp -> Type
chainOpTypes (EQProp l) = TFunc (TBase TBool l) (TFunc (TBase TBool l) (TBase TBool l) l) l
chainOpTypes (EQPropU l) = TFunc (TBase TBool l) (TFunc (TBase TBool l) (TBase TBool l) l) l
chainOpTypes (EQ l) = TFunc (TBase TInt l) (TFunc (TBase TInt l) (TBase TBool l) l) l
chainOpTypes (NEQ l) = TFunc (TBase TInt l) (TFunc (TBase TInt l) (TBase TBool l) l) l
chainOpTypes (NEQU l) = TFunc (TBase TInt l) (TFunc (TBase TInt l) (TBase TBool l) l) l
chainOpTypes (LTE l) = TFunc (TBase TInt l) (TFunc (TBase TInt l) (TBase TBool l) l) l
chainOpTypes (LTEU l) = TFunc (TBase TInt l) (TFunc (TBase TInt l) (TBase TBool l) l) l
chainOpTypes (GTE l) = TFunc (TBase TInt l) (TFunc (TBase TInt l) (TBase TBool l) l) l
chainOpTypes (GTEU l) = TFunc (TBase TInt l) (TFunc (TBase TInt l) (TBase TBool l) l) l
chainOpTypes (LT l) = TFunc (TBase TInt l) (TFunc (TBase TInt l) (TBase TBool l) l) l
chainOpTypes (GT l) = TFunc (TBase TInt l) (TFunc (TBase TInt l) (TBase TBool l) l) l

arithOpTypes :: ArithOp -> Type
arithOpTypes (Implies l) = TFunc (TBase TBool l) (TFunc (TBase TBool l) (TBase TBool l) l) l
arithOpTypes (ImpliesU l) = TFunc (TBase TBool l) (TFunc (TBase TBool l) (TBase TBool l) l) l
arithOpTypes (Conj l) = TFunc (TBase TBool l) (TFunc (TBase TBool l) (TBase TBool l) l) l
arithOpTypes (ConjU l) = TFunc (TBase TBool l) (TFunc (TBase TBool l) (TBase TBool l) l) l
arithOpTypes (Disj l) = TFunc (TBase TBool l) (TFunc (TBase TBool l) (TBase TBool l) l) l
arithOpTypes (DisjU l) = TFunc (TBase TBool l) (TFunc (TBase TBool l) (TBase TBool l) l) l
arithOpTypes (Neg l) = TFunc (TBase TBool l) (TBase TBool l) l
arithOpTypes (NegU l) = TFunc (TBase TBool l) (TBase TBool l) l
arithOpTypes (Add l) = TFunc (TBase TInt l) (TFunc (TBase TInt l) (TBase TInt l) l) l
arithOpTypes (Sub l) = TFunc (TBase TInt l) (TFunc (TBase TInt l) (TBase TInt l) l) l
arithOpTypes (Mul l) = TFunc (TBase TInt l) (TFunc (TBase TInt l) (TBase TInt l) l) l
arithOpTypes (Div l) = TFunc (TBase TInt l) (TFunc (TBase TInt l) (TBase TInt l) l) l
arithOpTypes (Mod l) = TFunc (TBase TInt l) (TFunc (TBase TInt l) (TBase TInt l) l) l

quantOpTypes :: QuantOp -> Type
quantOpTypes (Sum l) = TFunc (TBase TInt l) (TFunc (TBase TInt l) (TBase TInt l) l) l
quantOpTypes (Forall l) = TFunc (TBase TBool l) (TFunc (TBase TBool l) (TBase TBool l) l) l
quantOpTypes (Exists l) = TFunc (TBase TBool l) (TFunc (TBase TBool l) (TBase TBool l) l) l
quantOpTypes (Max l) = TFunc (TBase TInt l) (TFunc (TBase TInt l) (TBase TInt l) l) l
quantOpTypes (Min l) = TFunc (TBase TInt l) (TFunc (TBase TInt l) (TBase TInt l) l) l
quantOpTypes (Hash l) = TFunc (TBase TBool l) (TBase TInt l) l

opTypes :: Op -> Type
opTypes (ChainOp op) = chainOpTypes op
opTypes (ArithOp op) = arithOpTypes op
opTypes (QuantOp op) = quantOpTypes op
