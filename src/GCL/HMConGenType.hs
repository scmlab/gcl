{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module GCL.HMType where
import Syntax.Concrete
import GHC.Generics (Generic)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Text.Lazy (Text)
import qualified Data.Text.Lazy as Text
import Data.Loc
import Control.Monad.Except
import Control.Monad.RWS hiding (Sum)
import Prelude hiding (Ordering (..))

------------------------------------------
-- type enviornment
------------------------------------------

type TVar = Text

newtype TypeEnv = TypeEnv (Map TVar Scheme)

-- schemes model polymorphic types
data Scheme = ForallV [TVar] Type

emptyEnv :: TypeEnv
emptyEnv = TypeEnv Map.empty

extend :: TypeEnv -> (TVar, Scheme) -> TypeEnv
extend (TypeEnv env) (x, s) = TypeEnv (Map.insert x s env)

extend' :: TypeEnv -> (TVar, Scheme) -> Either TypeError TypeEnv
extend' e@(TypeEnv env) (x, s) =
  case Map.lookup x env of
    Nothing -> Right $ e `extend` (x, s)
    Just _ -> Left DuplicatedType

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
  apply s t@(TVar x _) = Map.findWithDefault t x s

  fv (TBase _ _) = Set.empty 
  fv (TArray _ t _) = fv t
  fv (TFunc t1 t2 _) = fv t1 `Set.union` fv t2
  fv (TVar x _) = Set.singleton x

instance Substitutable Scheme where
  apply s (ForallV vs t) = ForallV vs $ apply (foldl (flip Map.delete) s vs) t

  fv (ForallV vs t) = fv t `Set.difference` Set.fromList vs

instance Substitutable TypeEnv where
  apply s (TypeEnv env) = TypeEnv (Map.map (apply s) env)

  fv (TypeEnv env) = foldl (flip (Set.union . fv)) Set.empty (Map.elems env)

occursT :: (Substitutable a) => TVar -> a -> Bool
occursT x s = x `Set.member` fv s

------------------------------------------
-- type inference
------------------------------------------

type Infer = RWST TypeEnv [Constraint] InferState (Except TypeError)

type InferState = Int

data TypeError
  = NotInScope
  | UnifyFailed
  | RecursiveType
  | NotFunction
  | DuplicatedType
  deriving (Show, Eq, Generic)

initInfer :: InferState
initInfer = 0

runInfer :: TypeEnv -> Infer Type -> Either TypeError (Type, [Constraint])
runInfer env m = runExcept $ evalRWST m env initInfer

infer :: Expr -> Infer Type
infer (Lit lit l) = return (litTypes lit l)
infer (Var x _) = lookupEnv x
infer (Const c _) = lookupEnv c
infer (Op o l) = return (opTypes o l)
infer (App e1 e2 l) = do
  t1 <- infer e1
  t2 <- infer e2
  v <- fresh l
  uni t1 (TFunc t2 v l)
  return v
infer (Lam x e l) = do
  v <- fresh l
  t <- inEnv (x, ForallV [] v) (infer e)
  return (TFunc v t l)
infer (Hole l) = fresh l
infer (Quant op _ rng t l) = do
  x <- fresh l
  to <- infer op
  tr <- infer rng
  tt <- infer t
  uni to (TFunc x (TFunc x x l) l)
  uni tr (TBase TBool l)
  uni tt x
  return x
infer (Subst expr sub) = do
  t <- infer expr
  s <- mapM infer sub
  return $ apply s t

inferExpr :: TypeEnv -> Expr -> Either TypeError Scheme
inferExpr env e = 
  case runInfer env (infer e) of
    Left err -> Left err
    Right (t, cs) -> 
      case runSolver cs of
        Left err -> Left err
        Right s -> Right $ closeOver s t

inferDecl :: Declaration -> TypeEnv -> Either TypeError TypeEnv
inferDecl (ConstDecl ns t _ _) env = 
  foldl f (Right env) [n | Name n _ <- ns]
  where
    f (Left err) _ = Left err
    f (Right env') n = env' `extend'` (n, generalize env' t)
inferDecl (VarDecl ns t _ _) env = 
  foldl f (Right env) [n | Name n _ <- ns]
  where
    f (Left err) _ = Left err
    f (Right env') n = env' `extend'` (n, generalize env' t)
inferDecl (LetDecl (Name n _) args expr _) env = 
  case inferExpr env expr' of
    Left err -> Left err
    Right s -> env `extend'` (n, s)
  where
    expr' = foldr (\a e' -> Lam a e' NoLoc) expr args

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
lookupEnv (Name n _) = do
  (TypeEnv env) <- ask
  case Map.lookup n env of
    Just s -> instantiate s
    Nothing -> throwError NotInScope

inEnv :: (TVar, Scheme) -> Infer a -> Infer a
inEnv (x, sc) m = do
  let scope (TypeEnv e) = TypeEnv (Map.delete x e) `extend` (x, sc)
  local scope m

freshTVar :: Infer TVar
freshTVar = do
  i <- get
  put (i + 1)
  return . Text.pack $ ("x" ++ show i)

fresh :: Loc -> Infer Type
fresh l = flip TVar l <$> freshTVar

------------------------------------------
-- type check
------------------------------------------

checkName :: TypeEnv -> (Text, Expr) -> Either TypeError ()
checkName env@(TypeEnv envM) (n, expr) = 
  case (Map.lookup n envM, inferExpr env expr) of
    (Nothing, _) -> throwError NotInScope
    (_, Left err) -> throwError err
    (Just (ForallV _ t), Right (ForallV _ t')) -> void $ runSolver [(t, t')] 

checkExpr :: TypeEnv -> Expr -> Either TypeError ()
checkExpr env expr = void $ inferExpr env expr

checkGdCmd :: TypeEnv -> GdCmd -> Either TypeError ()
checkGdCmd env (GdCmd expr stmts _) = 
  inferExpr env expr >> mapM_ (checkStmt env) stmts

checkStmt :: TypeEnv -> Stmt -> Either TypeError ()
checkStmt _ (Skip _) = Right ()
checkStmt _ (Abort _) = Right ()
checkStmt env (Assign ns es _)
  | length ns > length es = throwError $ error "Missing Expression"
  | length ns < length es = throwError $ error "Duplicated Assignment"
  | otherwise = forM_ (zip ns es) (\(Name n _, expr) -> checkName env (n, expr))
checkStmt env (Assert expr _) = void $ inferExpr env expr
checkStmt env (LoopInvariant e1 e2 _) = void $ inferExpr env e1 >> inferExpr env e2
checkStmt env (Do gdcmds _) = mapM_ (checkGdCmd env) gdcmds
checkStmt env (If gdcmds _) = mapM_ (checkGdCmd env) gdcmds
checkStmt _ (SpecQM _) = Right ()
checkStmt _ (Spec _) = Right ()
checkStmt _ (Proof _) = Right ()

checkProg :: Program -> Either TypeError ()
checkProg (Program decls exprs defs stmts _) = do
  env <- foldM (flip inferDecl) emptyEnv decls
  mapM_ (checkExpr env) exprs
  mapM_ (checkName env) (Map.toList defs)
  mapM_ (checkStmt env) stmts

------------------------------------------
-- unification
------------------------------------------

type Constraint = (Type, Type)

type Unifier = (SubstT, [Constraint])

type Solve = Except TypeError

emptyUnifier :: Unifier
emptyUnifier = (emptySubstT, [])

unifiesScheme :: Scheme -> Scheme -> Solve SubstT
unifiesScheme (ForallV _ t1) (ForallV _ t2) = unifies t1 t2

unifies :: Type -> Type -> Solve SubstT
unifies (TBase t1 _) (TBase t2 _)
  | t1 == t2 = return emptySubstT
unifies (TArray i1 t1 _) (TArray i2 t2 _)
  | i1 == i2 = unifies t1 t2
unifies (TFunc t1 t2 _) (TFunc t3 t4 _) = do
  s1 <- unifies t1 t3
  s2 <- unifies (apply s1 t2) (apply s1 t4)
  return (s2 `compose` s1)
unifies (TVar x _) t = x `bind` t
unifies t (TVar x _) = x `bind` t
unifies _ _ = throwError UnifyFailed

runSolver :: [Constraint] -> Either TypeError SubstT
runSolver cs = runExcept . solver $ (emptySubstT, cs)

-- runSolver' :: (Scheme, Scheme) ->

solver :: Unifier -> Solve SubstT
solver (s, []) = return s
solver (s, (t1, t2) : cs) = do
  su <- unifies t1 t2
  solver (su `compose` s, map (fmap (apply su)) cs)

bind :: TVar -> Type -> Solve SubstT
x `bind` (TVar y _)
  | x == y = return emptySubstT
x `bind` t
  | occursT x t = throwError RecursiveType
  | otherwise = return (Map.singleton x t)

------------------------------------------
-- helper functions
------------------------------------------

litTypes :: Lit -> Loc -> Type 
litTypes (Num _) l = TBase TInt l
litTypes (Bol _) l = TBase TBool l
litTypes (Chr _) l = TBase TChar l

opTypes :: Op -> Loc -> Type
opTypes EQ l = TFunc (TBase TInt l) (TFunc (TBase TInt l) (TBase TBool l) l) l
opTypes NEQ l = TFunc (TBase TInt l) (TFunc (TBase TInt l) (TBase TBool l) l) l
opTypes LTE l = TFunc (TBase TInt l) (TFunc (TBase TInt l) (TBase TBool l) l) l
opTypes GTE l = TFunc (TBase TInt l) (TFunc (TBase TInt l) (TBase TBool l) l) l
opTypes LT l = TFunc (TBase TInt l) (TFunc (TBase TInt l) (TBase TBool l) l) l
opTypes GT l = TFunc (TBase TInt l) (TFunc (TBase TInt l) (TBase TBool l) l) l
opTypes Add l = TFunc (TBase TInt l) (TFunc (TBase TInt l) (TBase TInt l) l) l
opTypes Sub l = TFunc (TBase TInt l) (TFunc (TBase TInt l) (TBase TInt l) l) l
opTypes Mul l = TFunc (TBase TInt l) (TFunc (TBase TInt l) (TBase TInt l) l) l
opTypes Div l = TFunc (TBase TInt l) (TFunc (TBase TInt l) (TBase TInt l) l) l
opTypes Mod l = TFunc (TBase TInt l) (TFunc (TBase TInt l) (TBase TInt l) l) l
opTypes Implies l = TFunc (TBase TBool l) (TFunc (TBase TBool l) (TBase TBool l) l) l
opTypes Conj l = TFunc (TBase TBool l) (TFunc (TBase TBool l) (TBase TBool l) l) l
opTypes Disj l = TFunc (TBase TBool l) (TFunc (TBase TBool l) (TBase TBool l) l) l
opTypes Neg l = TFunc (TBase TBool l) (TBase TBool l) l
opTypes Sum l = _
opTypes Forall l = _
opTypes Exists l = _
