{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}
module GCL.Type where

import Control.Monad.Except
import Control.Monad.RWS hiding (Sum)
import Data.Aeson (ToJSON)
import Data.List.NonEmpty (NonEmpty)
import qualified Data.List.NonEmpty as NE
import Data.Loc
import qualified Data.Map as Map
import GHC.Generics (Generic)
import Syntax.Abstract
import Syntax.Common
import Prelude hiding (Ordering (..))
import GCL.Common
import Control.Monad.State (StateT(..), evalStateT)
import Syntax.Abstract.Util (bindingsToExpr)

data TypeError
  = NotInScope Name Loc
  | UnifyFailed Type Type Loc
  | RecursiveType Name Type Loc
  | NotFunction Type Loc
  | NotArray    Type Loc
  | NotEnoughExprsInAssigment (NonEmpty Name) Loc
  | TooManyExprsInAssigment (NonEmpty Expr) Loc
  | AssignToConst Name Loc
  | AssignToLet Name Loc
  deriving (Show, Eq, Generic)

instance ToJSON TypeError

instance Located TypeError where
  locOf (NotInScope _ l) = l
  locOf (UnifyFailed _ _ l) = l
  locOf (RecursiveType _ _ l) = l
  locOf (NotFunction _ l) = l
  locOf (NotArray _ l) = l
  locOf (NotEnoughExprsInAssigment _ l) = l
  locOf (TooManyExprsInAssigment _ l) = l
  locOf (AssignToConst _ l) = l
  locOf (AssignToLet _ l) = l

------------------------------------------
-- type enviornment
------------------------------------------

extend :: Env Type -> (Name, Type) -> TM (Env Type)
extend env (x, s) =
  case Map.lookup x env of
    Nothing -> return $ Map.insert x s env
    Just t -> throwError $ RecursiveType x t (locOf t)

------------------------------------------
-- Substitution
------------------------------------------

typeWithLoc :: Loc -> Type -> Type
typeWithLoc l (TBase b _) = TBase b l
typeWithLoc l (TArray i t _) = TArray i t l
typeWithLoc l (TFunc t1 t2 _) = TFunc t1 t2 l
typeWithLoc l (TVar n _) = TVar n l

------------------------------------------
-- type inference
------------------------------------------

type Infer = RWST (Env Type) [Constraint] FreshState (Except TypeError)
type TM = StateT FreshState (Except TypeError)


instance Fresh Infer where
  fresh = do
    i <- get
    (put . succ) i
    return i

runSolver :: Env Type ->  Infer Type -> TM (Type, [Constraint])
runSolver = toEvalStateT

unify :: Type -> Type -> Infer ()
unify x y = tell [(x, y)]

inEnv :: [(Name, Type)] -> Infer Type -> Infer Type
inEnv l m = do
  let scope e = foldl (\e' (x, sc) -> Map.insert x sc e') e l
  local scope m

infer :: Expr -> Infer Type
infer (Paren expr _) = infer expr
infer (Lit lit l) = return (litTypes lit l)
infer (Var x _) = lookupInferEnv x
infer (Const c _) = lookupInferEnv c
infer (Op o) = inferOpTypes o
infer (Chain a op b loc) = do
  ta <- infer a
  top <- inferOpTypes op
  tb <- infer b

  case (a, b) of
    (Chain _ _ l _, Chain r _ _ _) -> do
      tl <- infer l
      tr <- infer r
      -- check type of `l op r` is bool
      unify top (TFunc tl (TFunc tr (TBase TBool (locOf op)) (locOf r)) (l <--> r))
    (Chain _ _ l _, _) -> do
      tl <- infer l
      unify top (TFunc tl (TFunc tb (TBase TBool (locOf op)) (locOf b)) (l <--> b))
    (_, Chain r _ _ _) -> do
      tr <- infer r
      unify top (TFunc ta (TFunc tr (TBase TBool (locOf op)) (locOf r)) (a <--> r))
    (_, _) -> do
      unify top (TFunc ta (TFunc tb (TBase TBool (locOf op)) (locOf b)) (a <--> b))
  return (TBase TBool loc)
infer (App e1 e2 l) = do
  t1 <- infer e1
  t2 <- infer e2
  v <- freshVar l
  unify t1 (TFunc t2 v l)
  return v
infer (Lam x e l) = do
  v <- freshVar l
  t <- inEnv [(x, v)] (infer e)
  return (TFunc v t l)
infer (Quant qop iters rng t l) = do
  tr <- inEnv [(n, TBase TInt (locOf n)) | n <- iters] (infer rng)
  unify tr (TBase TBool (locOf rng))

  tt <- inEnv [(n, TBase TInt (locOf n)) | n <- iters] (infer t)
  case qop of
    Op (ArithOp (Hash _)) -> do
      unify tt (TBase TBool (locOf t))
      return (TBase TInt l)
    op -> do
      to <- infer op
      x <- freshVar l
      unify to (TFunc x (TFunc x x (locOf qop)) (locOf qop))
      unify tt x
      return x
infer (Subst expr sub _) = do
  t <- infer expr
  s <- mapM infer (Map.map bindingsToExpr sub)
  return $ subst s t
infer (Click _ after) = infer after
infer (ArrIdx e1 e2 l) = do
  t1 <- infer e1
  let interval = case t1 of
        TArray itv _ _ -> itv
        _ -> emptyInterval
  t2 <- infer e2
  unify t2 (TBase TInt l)
  v <- freshVar l
  unify t1 (TArray interval v l)
  return v
infer (ArrUpd e1 e2 e3 l) = do
  t1 <- infer e1
  let interval = case t1 of
        TArray itv _ _ -> itv
        _ -> emptyInterval
  t2 <- infer e2
  t3 <- infer e3
  unify t2 (TBase TInt l)
  unify t1 (TArray interval t3 l)
  return t1

emptyInterval :: Interval
emptyInterval = Interval (Including zero) (Excluding zero) NoLoc
  where zero = Lit (Num 0) NoLoc

inferExpr :: Env Type -> Expr -> TM Type
inferExpr env e = do
  (t, cs) <- runSolver env (infer e)
  s <- solveConstraints cs
  return $ subst s t

inferDeclBody :: Env Type -> DeclBody -> TM ()
inferDeclBody env (DeclBody n args expr) = do
  let expr' = foldr (\a e' -> Lam a e' (a <--> e')) expr args
  case Map.lookup n env of
    Nothing -> throwError (NotInScope n (locOf n))
    Just t -> checkIsType env expr' t

inferDecl' :: Env Type -> [Name] -> Type -> Maybe Expr -> TM (Env Type)
inferDecl' env ns t p = do
  checkType env t
  env' <- foldM f env ns
  forM_ p (checkPredicate env')
  return env'
  where
    f env' n = env' `extend` (n, t)

inferDecl :: Env Type -> Declaration -> TM (Env Type)
inferDecl env (ConstDecl ns t p _) = inferDecl' env ns t p
inferDecl env (VarDecl ns t p _) = inferDecl' env ns t p
inferDecl env (LetDecl (DeclBody n args expr) _) = do
  let expr' = foldr (\a e' -> Lam a e' (a <--> e')) expr args
  s <- inferExpr env expr'
  env `extend` (n, s)
inferDecl env (BlockDecl ns t p ds _) = do
  env' <- inferDecl' env ns t p
  mapM_ (inferDeclBody env') ds
  return env'

lookupInferEnv :: Name -> Infer Type
lookupInferEnv n = do
  env <- ask
  maybe
    (throwError $ NotInScope n (locOf n))
    (return . typeWithLoc (locOf n))
    (Map.lookup n env)

freshVar :: Loc -> Infer Type
freshVar l = do
  TVar <$> freshName l <*> pure l

------------------------------------------
-- type check
------------------------------------------

checkAssign :: Env Type -> (Name, Expr) -> TM ()
checkAssign env (n, expr) =
  case Map.lookup n env of
    Nothing -> throwError $ NotInScope n (locOf n)
    Just t -> do
      checkIsType env expr t

checkIsType :: Env Type -> Expr -> Type -> TM ()
checkIsType env expr t = do
  (eType, cs) <- runSolver env (infer expr)
  void $ solveConstraints (cs `mappend` [(eType, t)])

checkPredicate :: Env Type -> Expr -> TM ()
checkPredicate env p =
  checkIsType env p (TBase TBool NoLoc)

checkType :: Env Type -> Type -> TM ()
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

checkExpr :: Env Type -> Expr -> TM ()
checkExpr env expr = void $ inferExpr env expr

checkGdCmd :: Env Type -> GdCmd -> TM ()
checkGdCmd env (GdCmd expr stmts _) = do
  checkPredicate env expr
  mapM_ (checkStmt env) stmts

checkStmt :: Env Type -> Stmt -> TM ()
checkStmt _ (Skip _) = return ()
checkStmt _ (Abort _) = return ()
checkStmt env (Assign ns es loc) -- NOTE : Not sure if Assign work this way
  | length ns > length es = let extraVars = drop (length es) ns in
    throwError $
      NotEnoughExprsInAssigment
        (NE.fromList extraVars)
        -- (locOf $ mergeRangesUnsafe (fromLocs $ map locOf extraVars))
        loc
  | length ns < length es = let extraExprs = drop (length ns) es in
    throwError $
      TooManyExprsInAssigment
        (NE.fromList extraExprs)
        -- (locOf $ mergeRangesUnsafe (fromLocs $ map locOf extraExprs))
        loc
  | otherwise = forM_ (zip ns es) (checkAssign env)
checkStmt env (AAssign x i e _) = do
  tx <- inferExpr env x
  case tx of
   TArray _ t _ -> do
      checkIsType env i (tInt NoLoc)
      checkIsType env e t
   _ -> throwError $ NotArray tx (locOf x)
checkStmt env (Assert expr _) = do
  checkPredicate env expr
checkStmt env (LoopInvariant e1 e2 _) = do
  checkPredicate env e1
  checkIsType env e2 (TBase TInt NoLoc)
checkStmt env (Do gdcmds _) = mapM_ (checkGdCmd env) gdcmds
checkStmt env (If gdcmds _) = mapM_ (checkGdCmd env) gdcmds
checkStmt _ (Spec _ _) = return ()
checkStmt _ (Proof _ _) = return ()
checkStmt env (Alloc x es l) =
  case Map.lookup x env of
    Nothing -> throwError $ NotInScope x (locOf x)
    Just (TBase TInt _) ->
      mapM_ (\e -> checkIsType env e (tInt NoLoc)) es
    Just t -> throwError (UnifyFailed t (tInt NoLoc) l)
checkStmt env (HLookup x e l) =
  case Map.lookup x env of
    Nothing -> throwError $ NotInScope x (locOf x)
    Just (TBase TInt _) ->
      checkIsType env e (tInt NoLoc)
    Just t -> throwError (UnifyFailed t (tInt NoLoc) l)
checkStmt env (HMutate e1 e2 _) = do
  checkIsType env e1 (tInt NoLoc)
  checkIsType env e2 (tInt NoLoc)
checkStmt env (Dispose e _) =
  checkIsType env e (tInt NoLoc)

-- declsMap :: [Declaration] -> Map Name (Either Type Expr)
-- declsMap [] = mempty
-- declsMap (ConstDecl ns t _ _ : decls) =
--   foldl (\m n -> Map.insert n (Left t) m) (declsMap decls) ns
-- declsMap (VarDecl ns t _ _ : decls) =
--   foldl (\m n -> Map.insert n (Left t) m) (declsMap decls) ns
-- declsMap (LetDecl n xs body _ : decls) =
--   Map.insert n (Right expr') (declsMap decls)
--   where
--     expr' = foldr (\x b -> Lam x b (b <--> locOf x)) body xs

-- NOTE : should this be check here?
checkIsVarAssign :: [Declaration] -> Stmt -> TM ()
checkIsVarAssign declarations (Assign ns _ _) =
  let (_, cs, ls) = splitDecls declarations in
  forM_ ns (\n ->
    if n `elem` cs
    then throwError $ AssignToConst n (locOf n)
    else when (n `elem` ls) $ throwError $ AssignToLet n (locOf n)
  )
  where
    splitDecls [] = ([], [], [])
    splitDecls (d : ds) = let (vs, cs, ls) = splitDecls ds in
      case d of
        VarDecl n _ _ _ -> (n ++ vs, cs, ls)
        ConstDecl n _ _ _ -> (vs, n ++ cs, ls)
        LetDecl (DeclBody n _ _) _  -> (vs, cs, n : ls)
        BlockDecl n _ _ _ _ -> (vs, n ++ cs, ls)
checkIsVarAssign _ _ = return ()

checkProg :: Program -> TM ()
checkProg (Program decls exprs defs stmts _) = do
  mapM_ (checkIsVarAssign decls) stmts
  env <- foldM inferDecl emptyEnv decls
  mapM_ (checkExpr env) exprs
  mapM_ (checkAssign env) (Map.toList defs)
  mapM_ (checkStmt env) stmts

runTM :: TM a -> Either TypeError a
runTM p = runExcept (evalStateT p initFreshState)

------------------------------------------
-- unification
------------------------------------------

type Constraint = (Type, Type)

type Unifier = (Subs Type, [Constraint])

emptyUnifier :: Unifier
emptyUnifier = (emptySubs, [])

unifies :: Type -> Type -> TM (Subs Type)
unifies (TBase t1 _) (TBase t2 _)
  | t1 == t2 = return emptySubs
unifies (TArray _ t1 _) (TArray _ t2 _) =
  unifies t1 t2   {-  | i1 == i2 = unifies t1 t2 -}
  -- SCM: for now, we do not check the intervals
-- view array of type `t` as function type of `Int -> t`
unifies (TArray _ t1 _) (TFunc (TBase TInt _) t2 _) =
  unifies t1 t2
unifies (TFunc t1 t2 _) (TFunc t3 t4 _) = do
  s1 <- unifies t1 t3
  s2 <- unifies (subst s1 t2) (subst s1 t4)
  return (s2 `compose` s1)
unifies (TVar x l) t = bind x t l
unifies t (TVar x _) = bind x t (locOf t)
unifies t1 t2 = throwError $ UnifyFailed t1 t2 (locOf t1)

solveConstraints :: [Constraint] -> TM (Subs Type)
solveConstraints cs = solveUnifier (emptySubs, cs)

solveUnifier :: Unifier -> TM (Subs Type)
solveUnifier (s, []) = return s
solveUnifier (s, c : cs) = do
  su <- uncurry unifies c
  solveUnifier (su `compose` s, map (f (subst su)) cs)
  where
    f g (a, b) = (g a, g b)

bind :: Name -> Type -> Loc -> TM (Subs Type)
bind x (TVar y _) _
  | x == y = return emptySubs
bind x t l
  | occurs x t = throwError $ RecursiveType x t l
  | otherwise = return (Map.singleton x (typeWithLoc l t))

------------------------------------------
-- helper functions
------------------------------------------

litTypes :: Lit -> Loc -> Type
litTypes (Num _) l = TBase TInt l
litTypes (Bol _) l = TBase TBool l
litTypes (Chr _) l = TBase TChar l
litTypes Emp     l = TBase TBool l

-- NOTE : EQ, NEQ, NEQU is redundant here
inferOpTypes :: Op -> Infer Type
inferOpTypes op = do
  case op of
    ChainOp (EQ l) -> f l
    ChainOp (NEQ l) -> f l
    ChainOp (NEQU l) -> f l
    _ -> return (opTypes op)
  where
    f l = do
      x <- freshVar l
      return (TFunc x (TFunc x (TBase TBool l) l) l)

tBool, tInt :: Loc -> Type
tBool = TBase TBool
tInt = TBase TInt

(.->) :: (Loc -> Type) -> (Loc -> Type) -> (Loc -> Type)
(t1 .-> t2) l = TFunc (t1 l) (t2 l) l
infixr 1 .->

chainOpTypes :: ChainOp -> Type
chainOpTypes (EQProp l)  = tBool .-> tBool .-> tBool $ l
chainOpTypes (EQPropU l) = tBool .-> tBool .-> tBool $ l
chainOpTypes (EQ l)      = tInt .-> tInt .-> tBool $ l
chainOpTypes (NEQ l)     = tInt .-> tInt .-> tBool $ l
chainOpTypes (NEQU l)    = tInt .-> tInt .-> tBool $ l
chainOpTypes (LTE l)     = tInt .-> tInt .-> tBool $ l
chainOpTypes (LTEU l)    = tInt .-> tInt .-> tBool $ l
chainOpTypes (GTE l)     = tInt .-> tInt .-> tBool $ l
chainOpTypes (GTEU l)    = tInt .-> tInt .-> tBool $ l
chainOpTypes (LT l)      = tInt .-> tInt .-> tBool $ l
chainOpTypes (GT l)      = tInt .-> tInt .-> tBool $ l

arithOpTypes :: ArithOp -> Type
arithOpTypes (Implies l)  = tBool .-> tBool .-> tBool $ l
arithOpTypes (ImpliesU l) = tBool .-> tBool .-> tBool $ l
arithOpTypes (Conj l)     = tBool .-> tBool .-> tBool $ l
arithOpTypes (ConjU l)    = tBool .-> tBool .-> tBool $ l
arithOpTypes (Disj l)     = tBool .-> tBool .-> tBool $ l
arithOpTypes (DisjU l)    = tBool .-> tBool .-> tBool $ l
arithOpTypes (Neg l)      = tBool .-> tBool $ l
arithOpTypes (NegU l)     = tBool .-> tBool $ l
arithOpTypes (Add l) = tInt .-> tInt .-> tInt $ l
arithOpTypes (Sub l) = tInt .-> tInt .-> tInt $ l
arithOpTypes (Mul l) = tInt .-> tInt .-> tInt $ l
arithOpTypes (Div l) = tInt .-> tInt .-> tInt $ l
arithOpTypes (Mod l) = tInt .-> tInt .-> tInt $ l
arithOpTypes (Max l) = tInt .-> tInt .-> tInt $ l
arithOpTypes (Min l) = tInt .-> tInt .-> tInt $ l
arithOpTypes (Exp l) = tInt .-> tInt .-> tInt $ l
arithOpTypes (Hash l) = tBool .-> tInt $ l
arithOpTypes (PointsTo l) = tInt .-> tInt .-> tInt $ l
arithOpTypes (SConj l)    = tBool .-> tBool .-> tBool $ l
arithOpTypes (SImp l)     = tBool .-> tBool .-> tBool $ l


opTypes :: Op -> Type
opTypes (ChainOp op) = chainOpTypes op
opTypes (ArithOp op) = arithOpTypes op
