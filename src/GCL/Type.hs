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
import Syntax.Abstract.Located ()
import Syntax.Common
import Prelude hiding (Ordering (..))
import GCL.Common
import Control.Monad.State (StateT(..), evalStateT)


data TypeError
  = NotInScope Name Loc
  | UnifyFailed Type Type Loc
  | RecursiveType Name Type Loc
  | NotFunction Type Loc
  | -- TODO: move these to scope checking 
    NotEnoughExprsInAssigment (NonEmpty Name) Loc
  | TooManyExprsInAssigment (NonEmpty Expr) Loc
  deriving (Show, Eq, Generic)

instance ToJSON TypeError

instance Located TypeError where
  locOf (NotInScope _ l) = l
  locOf (UnifyFailed _ _ l) = l
  locOf (RecursiveType _ _ l) = l
  locOf (NotFunction _ l) = l
  locOf (NotEnoughExprsInAssigment _ l) = l
  locOf (TooManyExprsInAssigment _ l) = l

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
infer (Paren expr) = infer expr
infer (Lit lit l) = return (litTypes lit l)
infer (Var x _) = lookupInferEnv x
infer (Const c _) = lookupInferEnv c
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
infer (Hole l) = freshVar l
infer (Quant qop iters rng t l) = do
  tr <- inEnv [(n, TBase TInt (locOf n)) | n <- iters] (infer rng)
  unify tr (TBase TBool (locOf rng))

  tt <- inEnv [(n, TBase TInt (locOf n)) | n <- iters] (infer t)
  case qop of
    Left (QuantOp (Hash _)) -> do
      unify tt (TBase TBool (locOf t))
      return (TBase TInt l)
    Left op -> do
      let to = opTypes op
      unifyQOp tt to
    Right qop' -> do
      to <- infer qop'
      unifyQOp tt to
  where
    unifyQOp tt to = do
      x <- freshVar l
      unify to (TFunc x (TFunc x x (locOf qop)) (locOf qop))
      unify tt x
      return x
infer (Subst expr sub _) = do
  t <- infer expr
  s <- mapM infer sub
  return $ subst s t

inferExpr :: Env Type -> Expr -> TM Type
inferExpr env e = do
  (t, cs) <- runSolver env (infer e)
  s <- solveConstraints cs
  return $ subst s t

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
inferDecl env (LetDecl n args expr _) = do
  let expr' = foldr (\a e' -> Lam a e' (a <--> e')) expr args
  s <- inferExpr env expr'
  env `extend` (n, s)

lookupInferEnv :: Name -> Infer Type
lookupInferEnv n = do
  env <- ask
  maybe
    (throwError $ NotInScope n (locOf n))
    (return . typeWithLoc (locOf n))
    (Map.lookup n env)

freshVar :: Loc -> Infer Type
freshVar l = do
  t <- freshText
  return (TVar (Name t l) l)

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
checkStmt env (Assert expr _) = do
  checkPredicate env expr
checkStmt env (LoopInvariant e1 e2 _) = do
  checkPredicate env e1
  checkIsType env e2 (TBase TInt NoLoc)
checkStmt env (Do gdcmds _) = mapM_ (checkGdCmd env) gdcmds
checkStmt env (If gdcmds _) = mapM_ (checkGdCmd env) gdcmds
checkStmt _ (Spec _ _) = return ()
checkStmt _ (Proof _) = return ()

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

checkProg :: Program -> TM ()
checkProg (Program decls exprs defs stmts _) = do
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
unifies (TArray i1 t1 _) (TArray i2 t2 _)
  | i1 == i2 = unifies t1 t2
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
      x <- freshVar l
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
arithOpTypes (Max l) = TFunc (TBase TInt l) (TFunc (TBase TInt l) (TBase TInt l) l) l
arithOpTypes (Min l) = TFunc (TBase TInt l) (TFunc (TBase TInt l) (TBase TInt l) l) l

quantOpTypes :: QuantOp -> Type
quantOpTypes (Sum l) = TFunc (TBase TInt l) (TFunc (TBase TInt l) (TBase TInt l) l) l
quantOpTypes (Forall l) = TFunc (TBase TBool l) (TFunc (TBase TBool l) (TBase TBool l) l) l
quantOpTypes (Exists l) = TFunc (TBase TBool l) (TFunc (TBase TBool l) (TBase TBool l) l) l
quantOpTypes (Hash l) = TFunc (TBase TBool l) (TBase TInt l) l

opTypes :: Op -> Type
opTypes (ChainOp op) = chainOpTypes op
opTypes (ArithOp op) = arithOpTypes op
opTypes (QuantOp op) = quantOpTypes op
