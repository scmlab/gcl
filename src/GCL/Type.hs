{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}

module GCL.Type where

import           Control.Monad.Reader
import           Control.Monad.Writer.Lazy
import           Control.Monad.State.Lazy
import           Control.Monad.Except
import           Data.Aeson                     ( ToJSON )
import           Data.Loc
import qualified Data.Map                      as Map
import           Data.Maybe                     ( fromJust )
import           Data.Bifunctor                 ( bimap
                                                , second
                                                )
import           GHC.Generics                   ( Generic )
import           Syntax.Abstract
import           Syntax.Abstract.Util
import           Syntax.Common
import           GCL.Common
import           GCL.Scope                      ( TypeInfo(..)
                                                , TypeDefnInfo(..)
                                                , lookupFst
                                                , collectIds
                                                )
import           Server.TokenMap                ( Scope )
import           Prelude                 hiding ( EQ
                                                , LT
                                                , GT
                                                )
import           Debug.Trace

data TypeError
    = NotInScope Name
    | UnifyFailed Type Type Loc
    | RecursiveType Name Type Loc
    | UndefinedType Name
    deriving (Show, Eq, Generic)

instance ToJSON TypeError

instance Located TypeError where
  locOf (NotInScope n       ) = locOf n
  locOf (UnifyFailed   _ _ l) = l
  locOf (RecursiveType _ _ l) = l
  locOf (UndefinedType n    ) = locOf n

type TypeInferM a
  =  forall m
   . ( MonadState FreshState m
     , MonadWriter [Constraint] m
     , MonadError TypeError m
     )
  => m a

type TypeCheckM a
  =  forall m
   . ( MonadReader (Scope TypeDefnInfo, Scope TypeInfo) m
     , MonadState FreshState m
     , MonadError TypeError m
     )
  => m a

--------------------------------------------------------------------------------
-- Type inference
class Located a => InferType a where
  inferType :: (Scope TypeDefnInfo, Scope TypeInfo) -> a -> TypeInferM Type

runInferType
  :: (MonadError TypeError m, InferType a)
  => (Scope TypeDefnInfo, Scope TypeInfo)
  -> a
  -> m (Subs Type, Type)
runInferType env x = do
  (t, constraints) <- evalStateT (runWriterT (inferType env x)) 0
  s                <- solveConstraints constraints
  return (s, subst s t)

instance InferType a => InferType [a] where
  inferType env xs = do
    ts <- mapM (inferType env) xs
    v  <- freshVar
    mapM_ (\(t, x) -> tell [(v, t, locOf x)]) (zip ts xs)
    return v


instance InferType Expr where
  inferType _   (Lit   lit l) = return (litTypes lit l)
  inferType env (Var   x   _) = inferType env x
  inferType env (Const x   _) = inferType env x
  inferType env (Op o       ) = inferType env o
  inferType env (App (App (Op op@(ChainOp _)) e1 _) e2 l) = do
    top <- inferType env op

    t1  <- case e1 of
      App (App (Op (ChainOp _)) _ _) e12 _ -> do
        _ <- inferType env e1
        inferType env e12
      _ -> inferType env e1

    t2 <- inferType env e2
    v  <- freshVar
    tell [(top, t1 ~-> t2 ~-> v, l)]

    return (tBool l)
  inferType env (App e1 e2 l) = do
    t1 <- inferType env e1
    t2 <- inferType env e2
    v  <- freshVar
    tell [(t1, t2 ~-> v, l)]
    return v
  inferType env (Lam x e l) = do
    v <- freshVar
    t <- inferType
      (second (Map.insert (nameToText x) (VarTypeInfo v (locOf x))) env)
      e
    return (TFunc v t l)
  inferType env (Quant qop iters rng t l) = do
    titers <- mapM (const freshVar) iters
    let localEnv = second
          (Map.fromList
            [ (nameToText n, VarTypeInfo tn (locOf n))
            | n  <- iters
            , tn <- titers
            ] `Map.union`
          )
          env
    tr <- inferType localEnv rng
    tell [(tr, tBool NoLoc, locOf rng)]
    tt <- inferType localEnv t
    case qop of
      Op (ArithOp (Hash _)) -> do
        tell [(tt, tBool NoLoc, locOf t)]
        return (tInt l)
      op -> do
        to <- inferType env op
        x  <- freshVar
        tell [(to, x ~-> x ~-> x, locOf op), (tt, x, locOf t)]
        return x
  inferType env (Redex (Rdx _ expr)) = inferType env expr
  inferType env (RedexStem n _ _ _ ) = inferType env n
  inferType env (ArrIdx e1 e2 _    ) = do
    t1 <- inferType env e1
    let interval = case t1 of
          TArray itv _ _ -> itv
          _              -> emptyInterval
    t2 <- inferType env e2
    tell [(t2, tInt NoLoc, locOf e2)]
    v <- freshVar
    tell [(t1, TArray interval v NoLoc, locOf t1)]
    return v
  inferType env (ArrUpd e1 e2 e3 _) = do
    t1 <- inferType env e1
    let interval = case t1 of
          TArray itv _ _ -> itv
          _              -> emptyInterval
    t2 <- inferType env e2
    t3 <- inferType env e3
    tell [(t2, tInt NoLoc, locOf e2), (t1, TArray interval t3 NoLoc, locOf e1)]
    return t1
  inferType env (Case expr cs _) = do
    te <- inferType env expr
    ts <- mapM (inferCaseConstructor te (locOf expr)) cs
    t  <- freshVar
    mapM_ (\(tc, c) -> tell [(t, tc, locOf c)]) (zip ts cs)
    return t
   where
    inferCaseConstructor tPatt l (CaseConstructor patt e) = do
      (localTypeInfo, tPatt') <- inferPatt env patt
      let localTypeInfo' =
            Map.union
              . Map.fromList
              . map (\(n, t) -> (nameToText n, VarTypeInfo t (locOf n)))
              $ localTypeInfo
      tell [(tPatt, tPatt', l)]
      inferType (second localTypeInfo' env) e

inferPatt
  :: (Scope TypeDefnInfo, Scope TypeInfo)
  -> Pattern
  -> TypeInferM ([(Name, Type)], Type)
inferPatt _ (PattLit    x) = return ([], litTypes x (locOf x))
inferPatt _ (PattBinder n) = do
  tn <- freshVar
  return ([(n, tn)], tn)
inferPatt _   (PattWildcard _         ) = ([], ) <$> freshVar
inferPatt env (PattConstructor n patts) = do
  tPatt              <- freshVar
  tn                 <- inferType env n
  (localEnv, tpatts) <- unzip <$> mapM (inferPatt env) patts

  tell [(tn, wrapTFunc tpatts tPatt, n <--> patts)]
  return (concat localEnv, tPatt)


instance InferType Op where
  inferType env (ChainOp op) = inferType env op
  inferType env (ArithOp op) = inferType env op

instance InferType ChainOp where
  inferType _ (EQProp  l) = return $ tBool .-> tBool .-> tBool $ l
  inferType _ (EQPropU l) = return $ tBool .-> tBool .-> tBool $ l
  inferType _ (EQ      l) = do
    x <- freshVar
    return $ const x .-> const x .-> tBool $ l
  inferType _ (NEQ  l) = return $ tInt .-> tInt .-> tBool $ l
  inferType _ (NEQU l) = return $ tInt .-> tInt .-> tBool $ l
  inferType _ (LTE  l) = return $ tInt .-> tInt .-> tBool $ l
  inferType _ (LTEU l) = return $ tInt .-> tInt .-> tBool $ l
  inferType _ (GTE  l) = return $ tInt .-> tInt .-> tBool $ l
  inferType _ (GTEU l) = return $ tInt .-> tInt .-> tBool $ l
  inferType _ (LT   l) = do
    x <- freshVar
    return $ const x .-> const x .-> tBool $ l
  inferType _ (GT l) = do
    x <- freshVar
    return $ const x .-> const x .-> tBool $ l

instance InferType ArithOp where
  inferType _ (Implies  l) = return $ tBool .-> tBool .-> tBool $ l
  inferType _ (ImpliesU l) = return $ tBool .-> tBool .-> tBool $ l
  inferType _ (Conj     l) = return $ tBool .-> tBool .-> tBool $ l
  inferType _ (ConjU    l) = return $ tBool .-> tBool .-> tBool $ l
  inferType _ (Disj     l) = return $ tBool .-> tBool .-> tBool $ l
  inferType _ (DisjU    l) = return $ tBool .-> tBool .-> tBool $ l
  inferType _ (Neg      l) = return $ tBool .-> tBool $ l
  inferType _ (NegU     l) = return $ tBool .-> tBool $ l
  inferType _ (Add      l) = return $ tInt .-> tInt .-> tInt $ l
  inferType _ (Sub      l) = return $ tInt .-> tInt .-> tInt $ l
  inferType _ (Mul      l) = return $ tInt .-> tInt .-> tInt $ l
  inferType _ (Div      l) = return $ tInt .-> tInt .-> tInt $ l
  inferType _ (Mod      l) = return $ tInt .-> tInt .-> tInt $ l
  inferType _ (Max      l) = return $ tInt .-> tInt .-> tInt $ l
  inferType _ (Min      l) = return $ tInt .-> tInt .-> tInt $ l
  inferType _ (Exp      l) = return $ tInt .-> tInt .-> tInt $ l
  inferType _ (Hash     l) = return $ tBool .-> tInt $ l
  inferType _ (PointsTo l) = return $ tInt .-> tInt .-> tInt $ l
  inferType _ (SConj    l) = return $ tBool .-> tBool .-> tBool $ l
  inferType _ (SImp     l) = return $ tBool .-> tBool .-> tBool $ l

instance InferType Name where
  inferType env n = case Map.lookup (nameToText n) (snd env) of
    Just (TypeDefnCtorInfo t _) -> return t
    Just (FuncDefnInfo _ mt _ ) -> return . fromJust $ mt
    Just (ConstTypeInfo t _   ) -> return t
    Just (VarTypeInfo   t _   ) -> return t
    _                           -> throwError (NotInScope n)

--------------------------------------------------------------------------------
-- unification
type Constraint = (Type, Type, Loc)
type Unifier = (Subs Type, [Constraint])

emptyUnifier :: Unifier
emptyUnifier = (emptySubs, [])

unifies :: MonadError TypeError m => Type -> Type -> Loc -> m (Subs Type)
unifies (TBase t1 _) (TBase t2 _) _ | t1 == t2 = return emptySubs
unifies (TArray _ t1 _) (TArray _ t2 _) l = unifies t1 t2 l {-  | i1 == i2 = unifies t1 t2 -}
  -- SCM: for now, we do not check the intervals
-- view array of type `t` as function type of `Int -> t`
unifies (TArray _ t1 _) (TFunc (TBase TInt _) t2 _) l = unifies t1 t2 l
unifies (TFunc (TBase TInt _) t1 _) (TArray _ t2 _) l = unifies t1 t2 l
unifies (TFunc t1 t2 _) (TFunc t3 t4 _) l = do
  s1 <- unifies t1 t3 l
  s2 <- unifies (subst s1 t2) (subst s1 t4) l
  return (s2 `compose` s1)
unifies (TCon n1 args1 _) (TCon n2 args2 _) _
  | n1 == n2 && length args1 == length args2 = return emptySubs
unifies (TVar x1 _) (TVar x2 _) _ | x1 == x2 = return emptySubs
unifies (TVar x _) t@(TBase tb _) _ | x == baseToName tb =
  return $ Map.singleton x t
unifies (TVar x _) t@(TCon n args _) _ | x == n && null args =
  return $ Map.singleton x t
unifies t1 t2@(TVar _ _) l                   = unifies t2 t1 l
unifies (TMetaVar x) (TMetaVar y) _ | x == y = return emptySubs
unifies (TMetaVar x) t            l          = bind x t l
unifies t            (TMetaVar x) l          = bind x t l
unifies t1           t2           l          = throwError $ UnifyFailed t1 t2 l

bind :: MonadError TypeError m => Name -> Type -> Loc -> m (Subs Type)
bind x t l | occurs x t = throwError $ RecursiveType x t l
           | otherwise  = return (Map.singleton x t)

solveConstraints :: MonadError TypeError m => [Constraint] -> m (Subs Type)
solveConstraints cs = solveUnifier (emptySubs, cs)

solveUnifier :: MonadError TypeError m => Unifier -> m (Subs Type)
solveUnifier (s, []              ) = return s
solveUnifier (s, (t1, t2, l) : cs) = do
  su <- unifies t1 t2 l
  solveUnifier (su `compose` s, map (f (subst su)) cs)
  where f g (a, b, l') = (g a, g b, l')

--------------------------------------------------------------------------------
-- Type check
class TypeCheckable a where
    typeCheck :: a -> TypeCheckM ()

runTypeCheck :: Program -> Either TypeError ()
runTypeCheck prog =
  runExcept $ evalStateT (runReaderT (typeCheck prog) mempty) 0

generateEnv
  :: (MonadState FreshState m, MonadError TypeError m)
  => Program
  -> m (Scope TypeDefnInfo, Scope TypeInfo)
generateEnv prog = do
  let (tids, ids) = collectIds prog
  ids' <- mapM
    (\(t, info) -> case info of
      FuncDefnInfo exprs _ l -> do
        v <- freshVar
        return (t, FuncDefnInfo exprs (Just v) l)
      _ -> return (t, info)
    )
    ids
  return (Map.fromList tids, Map.fromList ids')
  --foldM
    --(\env' (t, info) -> case info of
      --FuncDefnInfo exprs _ l -> do
        --(subs, mt) <- runInferType env' exprs
        --return $ trace ("func defn " ++ show exprs) $ Map.insert
          --t
          --(FuncDefnInfo exprs (Just mt) l)
          --env'
      --_ -> return env'
    --)
    --(Map.fromList envList)
    --envList

infer :: InferType a => a -> TypeCheckM Type
infer x = ask >>= fmap snd . (`runInferType` x)

checkIsType :: InferType a => a -> Type -> TypeCheckM ()
checkIsType x t = do
  te <- infer x
  void $ solveConstraints [(t, te, locOf x)]

instance TypeCheckable a => TypeCheckable [a] where
  typeCheck xs = mapM_ typeCheck xs

instance TypeCheckable a => TypeCheckable (Maybe a) where
  typeCheck m = forM_ m typeCheck

instance TypeCheckable Program where
  typeCheck prog@(Program _ _ exprs stmts _) = do
    localEnv <- generateEnv prog
    local (bimap (Map.union (fst localEnv)) (Map.union (snd localEnv)))
          (typeCheck exprs)
    local (bimap (Map.union (fst localEnv)) (Map.union (snd localEnv)))
          (typeCheck stmts)

instance TypeCheckable Definition where
  typeCheck TypeDefn{}                = return ()
  typeCheck (FuncDefnSig n t mexpr _) = do
    env <- ask
    case Map.lookup (nameToText n) (snd env) of
      Just (ConstTypeInfo nt _) -> void $ solveConstraints [(t, nt, locOf n)]
      _                         -> throwError $ NotInScope n
    typeCheck mexpr
  typeCheck (FuncDefn n exprs) = return ()


instance TypeCheckable Stmt where
  typeCheck (Skip  _       ) = return ()
  typeCheck (Abort _       ) = return ()
  typeCheck (Assign ns es _) = do
    mapM_ checkAssign (zip ns es)
   where
    checkAssign :: (Name, Expr) -> TypeCheckM ()
    checkAssign (n, expr) = do
      env <- ask
      case Map.lookup (nameToText n) (snd env) of
        Just (VarTypeInfo t _) -> checkIsType expr t
        _                      -> throwError $ UndefinedType n
  typeCheck (AAssign x i e _) = do
    checkIsType i (tInt NoLoc)
    te <- infer e
    checkIsType
      x
      (TArray (Interval (Including i) (Including i) (locOf i)) te (locOf x))
  typeCheck (Assert expr _        ) = checkIsType expr (tBool NoLoc)
  typeCheck (LoopInvariant e1 e2 _) = do
    checkIsType e1 (tBool NoLoc)
    checkIsType e2 (tInt NoLoc)
  typeCheck (Do gds _)     = typeCheck gds
  typeCheck (If gds _)     = typeCheck gds
  typeCheck Spec{}         = return ()
  typeCheck Proof{}        = return ()
  typeCheck (Alloc x es _) = do
    env <- ask
    case Map.lookup (nameToText x) (snd env) of
      Just (VarTypeInfo t _) -> mapM_ (`checkIsType` t) es
      _                      -> throwError $ UndefinedType x
  typeCheck (HLookup x e _) = do
    env <- ask
    case Map.lookup (nameToText x) (snd env) of
      Just (VarTypeInfo   t _) -> checkIsType e t
      Just (ConstTypeInfo t _) -> checkIsType e t
      _                        -> throwError $ UndefinedType x
  typeCheck (HMutate e1 e2 _) = do
    infer e1 >>= checkIsType e2
  typeCheck (Dispose e _) = checkIsType e (tInt NoLoc)
  typeCheck (Block   p _) = typeCheck p

instance TypeCheckable GdCmd where
  typeCheck (GdCmd e s _) = checkIsType e (tBool NoLoc) >> typeCheck s

instance TypeCheckable Expr where
  typeCheck = void . infer

instance TypeCheckable Type where
  typeCheck TBase{}          = return ()
  typeCheck (TArray i  t  _) = typeCheck i >> typeCheck t
  typeCheck (TFunc  t1 t2 _) = typeCheck t1 >> typeCheck t2
  typeCheck (TCon   n  _  _) = do
    info <- lookupFst n
    case info of
      Just _ -> return ()
      _      -> throwError $ NotInScope n
  typeCheck TVar{}     = return ()
  typeCheck TMetaVar{} = return ()

instance TypeCheckable Interval where
  typeCheck (Interval e1 e2 _) = typeCheck e1 >> typeCheck e2

instance TypeCheckable Endpoint where
  typeCheck (Including e) = typeCheck e
  typeCheck (Excluding e) = typeCheck e


--------------------------------------------------------------------------------
-- helper combinators
freshVar :: MonadState FreshState m => m Type
freshVar = do
  TMetaVar <$> freshName "Type.metaVar" NoLoc

litTypes :: Lit -> Loc -> Type
litTypes (Num _) l = tInt l
litTypes (Bol _) l = tBool l
litTypes (Chr _) l = tChar l
litTypes Emp     l = tBool l

tBool, tInt, tChar :: Loc -> Type
tBool = TBase TBool
tInt = TBase TInt
tChar = TBase TChar

(.->) :: (Loc -> Type) -> (Loc -> Type) -> (Loc -> Type)
(t1 .-> t2) l = TFunc (t1 l) (t2 l) l
infixr 1 .->

(~->) :: Type -> Type -> Type
t1 ~-> t2 = TFunc t1 t2 NoLoc
infixr 1 ~->

emptyInterval :: Interval
emptyInterval = Interval (Including zero) (Excluding zero) NoLoc
  where zero = Lit (Num 0) NoLoc

chainOpTypes :: ChainOp -> Type
chainOpTypes (EQProp  l) = tBool .-> tBool .-> tBool $ l
chainOpTypes (EQPropU l) = tBool .-> tBool .-> tBool $ l
chainOpTypes (EQ      l) = tInt .-> tInt .-> tBool $ l
chainOpTypes (NEQ     l) = tInt .-> tInt .-> tBool $ l
chainOpTypes (NEQU    l) = tInt .-> tInt .-> tBool $ l
chainOpTypes (LTE     l) = tInt .-> tInt .-> tBool $ l
chainOpTypes (LTEU    l) = tInt .-> tInt .-> tBool $ l
chainOpTypes (GTE     l) = tInt .-> tInt .-> tBool $ l
chainOpTypes (GTEU    l) = tInt .-> tInt .-> tBool $ l
chainOpTypes (LT      l) = tInt .-> tInt .-> tBool $ l
chainOpTypes (GT      l) = tInt .-> tInt .-> tBool $ l


arithOpTypes :: ArithOp -> Type
arithOpTypes (Implies  l) = tBool .-> tBool .-> tBool $ l
arithOpTypes (ImpliesU l) = tBool .-> tBool .-> tBool $ l
arithOpTypes (Conj     l) = tBool .-> tBool .-> tBool $ l
arithOpTypes (ConjU    l) = tBool .-> tBool .-> tBool $ l
arithOpTypes (Disj     l) = tBool .-> tBool .-> tBool $ l
arithOpTypes (DisjU    l) = tBool .-> tBool .-> tBool $ l
arithOpTypes (Neg      l) = tBool .-> tBool $ l
arithOpTypes (NegU     l) = tBool .-> tBool $ l
arithOpTypes (Add      l) = tInt .-> tInt .-> tInt $ l
arithOpTypes (Sub      l) = tInt .-> tInt .-> tInt $ l
arithOpTypes (Mul      l) = tInt .-> tInt .-> tInt $ l
arithOpTypes (Div      l) = tInt .-> tInt .-> tInt $ l
arithOpTypes (Mod      l) = tInt .-> tInt .-> tInt $ l
arithOpTypes (Max      l) = tInt .-> tInt .-> tInt $ l
arithOpTypes (Min      l) = tInt .-> tInt .-> tInt $ l
arithOpTypes (Exp      l) = tInt .-> tInt .-> tInt $ l
arithOpTypes (Hash     l) = tBool .-> tInt $ l
arithOpTypes (PointsTo l) = tInt .-> tInt .-> tInt $ l
arithOpTypes (SConj    l) = tBool .-> tBool .-> tBool $ l
arithOpTypes (SImp     l) = tBool .-> tBool .-> tBool $ l
