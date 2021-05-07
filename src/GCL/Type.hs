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
import Data.Text (Text)
import qualified Data.Text as Text
import GHC.Generics (Generic)
import Syntax.Abstract
import Syntax.Common
import Prelude hiding (Ordering (..))

type TM = Except TypeError

------------------------------------------
-- type enviornment
------------------------------------------

type TVar = Text

newtype TypeEnv = TypeEnv (Map TVar Scheme) deriving (Eq, Show)

-- schemes model polymorphic types
data Scheme = ForallV [TVar] Type deriving (Eq, Show)

emptyEnv :: TypeEnv
emptyEnv = TypeEnv Map.empty

extend :: TypeEnv -> (TVar, Scheme) -> TypeEnv
extend (TypeEnv env) (x, s) = TypeEnv (Map.insert x s env)

extend' :: TypeEnv -> (TVar, Scheme) -> TM TypeEnv
extend' e@(TypeEnv env) (x, s) =
  case Map.lookup x env of
    Nothing -> return $ e `extend` (x, s)
    Just (ForallV _ t) -> throwError $ RecursiveType x t (locOf t)

------------------------------------------
-- Substitution
------------------------------------------

type SubstT = Map TVar Type

emptySubstT :: SubstT
emptySubstT = Map.empty

compose :: SubstT -> SubstT -> SubstT
s1 `compose` s2 = s1 `Map.union` Map.map (apply s1) s2

class Substitutable a where
  apply :: SubstT -> a -> a
  fv :: a -> Set TVar

instance Substitutable Type where
  apply _ t@(TBase _ _) = t
  apply s (TArray i t l) = TArray i (apply s t) l
  apply s (TFunc t1 t2 l) = TFunc (apply s t1) (apply s t2) l
  apply s t@(TVar (Name x _) _) = Map.findWithDefault t x s

  fv (TBase _ _) = Set.empty
  fv (TArray _ t _) = fv t
  fv (TFunc t1 t2 _) = fv t1 `Set.union` fv t2
  fv (TVar (Name x _) _) = Set.singleton x

instance Substitutable Scheme where
  apply s (ForallV vs t) = ForallV vs $ apply (foldl (flip Map.delete) s vs) t

  fv (ForallV vs t) = fv t `Set.difference` Set.fromList vs

instance Substitutable TypeEnv where
  apply s (TypeEnv env) = TypeEnv (Map.map (apply s) env)

  fv (TypeEnv env) = foldl (flip (Set.union . fv)) Set.empty (Map.elems env)

occursT :: (Substitutable a) => TVar -> a -> Bool
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
  = NotInScope Text Loc
  | UnifyFailed Type Type Loc
  | RecursiveType TVar Type Loc
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
infer (Chain a op b l) = do
  case a of
    Chain _ _ r _ -> do
      ta <- infer a
      -- check type of a is bool
      uni ta (TBase TBool l)

      tr <- infer r
      let top = chainOpTypes op
      tb <- infer b
      -- check type of `r op b` is bool
      uni top (TFunc tr (TFunc tb (TBase TBool NoLoc) (locOf b)) (r <--> b))
      
      return (TBase TBool l)
    _ -> do
      ta <- infer a
      let top = chainOpTypes op
      tb <- infer b
      uni top (TFunc ta (TFunc tb (TBase TBool NoLoc) (locOf b)) (a <--> b))
      return (TBase TBool (a <--> b))

infer (App e1 e2 l) = do
  t1 <- infer e1
  t2 <- infer e2
  v <- fresh l
  uni t1 (TFunc t2 v l)
  return v
infer (Lam x e l) = do
  v <- fresh l
  t <- inEnv [(nameToText x, ForallV [] v)] (infer e)
  return (TFunc v t l)
infer (Hole l) = fresh l
infer (Quant qop iters rng t l) = do
  x <- fresh l
  to <- case qop of
          Left op -> return (opTypes op)
          Right qop' -> infer qop'
  tr <- inEnv [(n, ForallV [] (TBase TInt loc)) | Name n loc <- iters] (infer rng)
  tt <- inEnv [(n, ForallV [] (TBase TInt loc)) | Name n loc <- iters] (infer t)
  uni to (TFunc x (TFunc x x l) l)
  uni tr (TBase TBool (locOf rng))
  uni tt x
  return x
infer (Subst expr sub) = do
  t <- infer expr
  s <- mapM infer sub
  return $ apply (Map.mapKeys nameToText s) t

inferExpr :: TypeEnv -> Expr -> TM Scheme
inferExpr env e = do
  (t, cs) <- runInfer env (infer e)
  s <- runSolver cs
  return $ closeOver s t

inferDecl :: TypeEnv -> Declaration -> TM TypeEnv
inferDecl env (ConstDecl ns t p _) = do
  checkType env t
  env' <- foldM f env [n | Name n _ <- ns]
  case p of
    Just prop -> checkExpr env' prop >> return env'
    Nothing -> return env'
  where
    f env' n = env' `extend'` (n, generalize env' t)
inferDecl env (VarDecl ns t p _) = do
  checkType env t
  env' <- foldM f env [n | Name n _ <- ns]
  case p of
    Just prop -> checkExpr env' prop >> return env'
    Nothing -> return env'
  where
    f env' n = env' `extend'` (n, generalize env' t)
inferDecl env (LetDecl (Name n _) args expr _) = do
  s <- inferExpr env expr'
  env `extend'` (n, s)
  where
    expr' = foldr (\a e' -> Lam a e' (locOf e')) expr args

instantiate :: Scheme -> Infer Type
instantiate (ForallV vs t) =
  flip apply t . Map.fromList . zip vs <$> mapM (const . fresh . locOf $ t) vs

generalize :: TypeEnv -> Type -> Scheme
generalize env t = ForallV (Set.toList $ fv t `Set.difference` fv env) t

closeOver :: SubstT -> Type -> Scheme
closeOver s t = generalize emptyEnv (apply s t)

uni :: Type -> Type -> Infer ()
uni t1 t2 = tell [(t1, t2)]

lookupEnv :: Name -> Infer Type
lookupEnv (Name n l) = do
  (TypeEnv env) <- ask
  case Map.lookup n env of
    Just (ForallV vs t) -> instantiate (ForallV vs (typeWithLoc l t))
    Nothing -> throwError $ NotInScope n l

inEnv :: [(TVar, Scheme)] -> Infer a -> Infer a
inEnv l m = do
  let scope (TypeEnv e) = TypeEnv $ foldl (\e' (x, sc) -> Map.insert x sc e') e l
  local scope m

freshTVar :: Infer TVar
freshTVar = do
  i <- get
  put (i + 1)
  return . Text.pack $ ("x" ++ show i)

fresh :: Loc -> Infer Type
fresh l = flip TVar l . flip Name l <$> freshTVar

------------------------------------------
-- type check
------------------------------------------

checkName :: TypeEnv -> (Name, Expr) -> TM ()
checkName env@(TypeEnv envM) (Name n l, expr) =
  case Map.lookup n envM of
    Nothing -> throwError $ NotInScope n l
    Just (ForallV _ t) -> do
      (ForallV _ t') <- inferExpr env expr
      void $ runSolver [(t, t')]

checkType :: TypeEnv -> Type -> TM ()
checkType _ (TBase _ _) = return ()
checkType env (TArray (Interval e1 e2 _) t _) = do
  ForallV _ t1 <- inferExpr env (getEndpointExpr e1)
  void $ runSolver [(t1, TBase TInt NoLoc)]
  ForallV _ t2 <- inferExpr env (getEndpointExpr e2)
  void $ runSolver [(t2, TBase TInt NoLoc)]
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
  ForallV _ gd <- inferExpr env expr
  void $ runSolver [(gd, TBase TBool NoLoc)]
  mapM_ (checkStmt env) stmts

checkStmt :: TypeEnv -> Stmt -> TM ()
checkStmt _ (Skip _) = return ()
checkStmt _ (Abort _) = return ()
checkStmt env (Assign ns es _)
  | length ns > length es = throwError $ error "Missing Expression"
  | length ns < length es = throwError $ error "Duplicated Assignment"
  | otherwise = forM_ (zip ns es) (checkName env)
checkStmt env (Assert expr _) = void $ inferExpr env expr
checkStmt env (LoopInvariant e1 e2 _) = void $ inferExpr env e1 >> inferExpr env e2
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

unifiesScheme :: Scheme -> Scheme -> TM SubstT
unifiesScheme (ForallV _ t1) (ForallV _ t2) = unifies t1 t2

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
unifies (TVar (Name x _) _) t = bind x t (locOf t)
unifies t (TVar (Name x _) l) = bind x t l
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

bind :: TVar -> Type -> Loc -> TM SubstT
bind x (TVar (Name y _) _) _
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

chainOpTypes :: ChainOp -> Type
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

opTypes :: Op -> Type
opTypes (ChainOp op) = chainOpTypes op
opTypes (ArithOp op) = arithOpTypes op
opTypes (QuantOp op) = quantOpTypes op
