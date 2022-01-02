{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE DeriveFunctor #-}


module GCL.Type where

import           Control.Applicative.Combinators
                                                ( (<|>) )
import           Control.Monad.Except
import           Control.Monad.State.Lazy
import           Control.Monad.Writer.Lazy
import           Data.Aeson                     ( ToJSON )
import           Data.Bifunctor                 ( first )
import           Data.Functor
import           Data.List
import           Data.Loc                       ( (<-->)
                                                , Loc(..)
                                                , Located
                                                , locOf
                                                )
import           Data.Loc.Range                 ( Range
                                                , fromLoc
                                                )
import           Data.Map                       ( Map )
import qualified Data.Map                      as Map
import           Data.Maybe                     ( fromMaybe )
import qualified Data.Text                     as Text
import           GCL.Common
import           GHC.Generics                   ( Generic )
import           Prelude                 hiding ( EQ
                                                , GT
                                                , LT
                                                )

import qualified Data.List.NonEmpty            as NonEmpty
import           Server.IntervalMap                ( IntervalMap )
import qualified Server.IntervalMap               as IntervalMap
import           Syntax.Abstract
import           Syntax.Abstract.Util
import           Syntax.Common

data ScopeTree a = ScopeTree
  { globalScope :: Map Name a             -- use name or range to Name to find corresponding type infos
  , localScopes :: IntervalMap (ScopeTree a)
  }
  deriving (Eq, Functor)

data ScopeTreeZipper a = ScopeTreeZipper
  { cursor  :: ScopeTree a
  , parents :: [(Range, ScopeTree a)]
  }
  deriving (Eq, Functor)

instance {-# Incoherent #-} Substitutable a b => Substitutable a (ScopeTree b) where
  subst s ScopeTree {..} =
    ScopeTree (subst s globalScope) (subst s localScopes)

instance {-# Incoherent #-} Substitutable a b => Substitutable a (ScopeTreeZipper b) where
  subst s ScopeTreeZipper {..} = ScopeTreeZipper (subst s cursor) parents

fsRootScopeTree :: ScopeTreeZipper a -> ScopeTreeZipper a
fsRootScopeTree s = maybe s fsRootScopeTree (fsUpScopeTree s)

fsUpScopeTree :: ScopeTreeZipper a -> Maybe (ScopeTreeZipper a)
fsUpScopeTree ScopeTreeZipper {..} = case parents of
  ((rng, p) : ps) -> Just $ ScopeTreeZipper
    (ScopeTree (globalScope p) (IntervalMap.insert rng cursor (localScopes p)))
    ps
  [] -> Nothing

insertScopeTree
  :: Range -> Map Name a -> ScopeTreeZipper a -> ScopeTreeZipper a
insertScopeTree rng m ScopeTreeZipper {..} =
  let cursor' = ScopeTree m mempty
  in  let parents' = (rng, cursor) : parents
      in  ScopeTreeZipper cursor' parents'

lookupScopeTree :: Name -> ScopeTreeZipper a -> Maybe a
lookupScopeTree n s =
  Map.lookup n (globalScope (cursor s))
    <|> (fsUpScopeTree s >>= lookupScopeTree n)

data TypeError
    = NotInScope Name
    | UnifyFailed Type Type Loc
    | RecursiveType Name Type Loc
    | AssignToConst Name
    | UndefinedType Name
    | DuplicatedIdentifiers [Name]
    | RedundantNames [Name]
    | RedundantExprs [Expr]
    | MissingArguments [Name]
    deriving (Show, Eq, Generic)

instance ToJSON TypeError

instance Located TypeError where
  locOf (NotInScope n               ) = locOf n
  locOf (UnifyFailed   _ _ l        ) = l
  locOf (RecursiveType _ _ l        ) = l
  locOf (AssignToConst         n    ) = locOf n
  locOf (UndefinedType         n    ) = locOf n
  locOf (DuplicatedIdentifiers ns   ) = locOf ns
  locOf (RedundantNames        ns   ) = locOf ns
  locOf (RedundantExprs        exprs) = locOf exprs
  locOf (MissingArguments      ns   ) = locOf ns

data TypeInfo =
    TypeDefnCtorInfo Type
    | ConstTypeInfo Type
    | VarTypeInfo Type
    deriving (Eq, Show)

instance Substitutable Type TypeInfo where
  subst s (TypeDefnCtorInfo t) = TypeDefnCtorInfo (subst s t)
  subst s (ConstTypeInfo    t) = ConstTypeInfo (subst s t)
  subst s (VarTypeInfo      t) = VarTypeInfo (subst s t)

newtype TypeDefnInfo = TypeDefnInfo [Name]
  deriving (Eq, Show)

dups :: Eq a => [a] -> [a]
dups = map head . filter ((> 1) . length) . group

duplicationCheck
  :: (Eq a, MonadError TypeError m) => [(Name, a)] -> m [(Name, a)]
duplicationCheck ns =
  let ds = dups ns
  in  if null ds
        then return ns
        else throwError $ DuplicatedIdentifiers (map fst ds)

class HasTypeDefnInfoScope m where
    getTypeDefnInfo :: m (ScopeTreeZipper TypeDefnInfo)
    setTypeDefnInfo :: ScopeTreeZipper TypeDefnInfo -> m ()

class HasTypeInfoScope m where
    getTypeInfo :: m (ScopeTreeZipper TypeInfo)
    setTypeInfo :: ScopeTreeZipper TypeInfo -> m ()

--------------------------------------------------------------------------------
-- Type inference

type TypeInferM = WriterT [Constraint] TypeCheckM

instance Fresh TypeInferM where
  getCounter = do
    (i, _, _) <- get
    return i
  setCounter i = do
    (_, s1, s2) <- get
    put (i, s1, s2)

instance HasTypeDefnInfoScope TypeInferM where
  getTypeDefnInfo = do
    (_, s, _) <- get
    return s
  setTypeDefnInfo s2 = do
    (s1, _, s3) <- get
    put (s1, s2, s3)

instance HasTypeInfoScope TypeInferM where
  getTypeInfo = do
    (_, _, s) <- get
    return s
  setTypeInfo s3 = do
    (s1, s2, _) <- get
    put (s1, s2, s3)

type Constraint = (Type, Type, Loc)

checkScope :: (MonadError TypeError m, Eq a) => [(Name, a)] -> m (Map Name a)
checkScope infos = Map.fromList <$> duplicationCheck infos

newScope
  :: (HasTypeDefnInfoScope m, HasTypeInfoScope m, MonadError TypeError m)
  => Loc
  -> ([(Name, TypeDefnInfo)], [(Name, TypeInfo)])
  -> m ()
newScope l (infos1, infos2) = do
  m1 <- checkScope infos1
  m2 <- checkScope infos2
  case fromLoc l of
    Just rng -> do
      getTypeDefnInfo >>= setTypeDefnInfo . insertScopeTree rng m1
      getTypeInfo >>= setTypeInfo . insertScopeTree rng m2
    _ -> pure ()

localScope
  :: (HasTypeDefnInfoScope m, HasTypeInfoScope m, MonadError TypeError m)
  => Loc
  -> ([(Name, TypeDefnInfo)], [(Name, TypeInfo)])
  -> m b
  -> m b
localScope l infos act = do
  newScope l infos
  x <- act
  getTypeDefnInfo >>= setTypeDefnInfo . fsUpScopeTree'
  getTypeInfo >>= setTypeInfo . fsUpScopeTree'
  return x
  where fsUpScopeTree' s = fromMaybe s (fsUpScopeTree s)

class Located a => InferType a where
    inferType :: a -> TypeInferM Type

runInferType :: InferType a => a -> TypeCheckM (Map Name Type, Type)
runInferType x = do
  (tx, constraints) <- runWriterT (inferType x)
  s                 <- solveConstraints constraints
  return (s, subst s tx)

instance InferType Expr where
  inferType (Lit   lit l) = return (litTypes lit l)
  inferType (Var   x   _) = inferType x
  inferType (Const x   _) = inferType x
  inferType (Op o       ) = inferType o
  inferType (App (App (Op op@(ChainOp _)) e1 _) e2 l) = do
    top <- inferType op

    t1  <- case e1 of
      App (App (Op (ChainOp _)) _ _) e12 _ -> do
        _ <- inferType e1
        inferType e12
      _ -> inferType e1

    t2 <- inferType e2
    v  <- freshVar
    tell [(top, t1 ~-> t2 ~-> v, l)]

    return (tBool l)
  inferType (App e1 e2 l) = do
    t1 <- inferType e1
    t2 <- inferType e2
    v  <- freshVar
    tell [(t1, t2 ~-> v, l)]
    return v
  inferType (Lam x e l) = do
    v <- freshVar
    t <- localScope l ([], [(x, VarTypeInfo v)]) (inferType e)
    return (TFunc v t l)

  inferType (Func name clauses l) = do
    -- infer the first clause
    t  <- inferFuncClause name (NonEmpty.head clauses)
    -- infer other clauses
    ts <- mapM (inferFuncClause name) clauses
    -- and unify them all

    let pairs = map (t, , l) (NonEmpty.toList ts)
    tell pairs

    return t
   where
      -- infer the types of arguments and the return type
    inferFuncClause :: Name -> FuncClause -> TypeInferM Type
    inferFuncClause _ (FuncClause patterns body) = do
      (binders', _) <- unzip <$> mapM collectPattInfos patterns
      let binders = concat binders'
      localScope l ([], binders) (inferType body)

  inferType (Tuple xs) = do
    ts <- mapM inferType xs
    return (TTuple ts)

  inferType (Quant qop ids rng t l) = do
    tids     <- mapM (const freshVar) ids
    (tr, tt) <- localScope
      l
      ([], [ (n, VarTypeInfo tn) | n <- ids, tn <- tids ])
      ((,) <$> inferType rng <*> inferType t)
    tell [(tr, tBool NoLoc, locOf rng)]
    case qop of
      Op (ArithOp (Hash _)) -> do
        tell [(tt, tBool NoLoc, locOf t)]
        return (tInt l)
      op -> do
        to <- inferType op
        x  <- freshVar
        tell [(to, x ~-> x ~-> x, locOf op), (tt, x, locOf t)]
        return x
  inferType (Redex (Rdx _ expr)) = inferType expr
  inferType (RedexStem n _ _ _ ) = inferType n
  inferType (ArrIdx e1 e2 _    ) = do
    t1 <- inferType e1
    let interval = case t1 of
          TArray itv _ _ -> itv
          _              -> emptyInterval
    t2 <- inferType e2
    tell [(t2, tInt NoLoc, locOf e2)]
    v <- freshVar
    tell [(t1, TArray interval v NoLoc, locOf t1)]
    return v
  inferType (ArrUpd e1 e2 e3 _) = do
    t1 <- inferType e1
    let interval = case t1 of
          TArray itv _ _ -> itv
          _              -> emptyInterval
    t2 <- inferType e2
    t3 <- inferType e3
    tell [(t2, tInt NoLoc, locOf e2), (t1, TArray interval t3 NoLoc, locOf e1)]
    return t1
  inferType (Case expr cs l) = do
    te <- inferType expr
    ts <- mapM inferType cs
    t  <- freshVar
    mapM_ (\(tc, c) -> tell [(TFunc te t l, tc, locOf c)]) (zip ts cs)
    return t

instance InferType CaseClause where
  inferType c@(CaseClause patt expr) = do
    (pattInfos, tPatt) <- collectPattInfos patt
    te                 <- localScope (locOf c) ([], pattInfos) (inferType expr)
    return (TFunc tPatt te (locOf c))

collectPattInfos :: Pattern -> TypeInferM ([(Name, TypeInfo)], Type)
collectPattInfos (PattLit    x) = return ([], litTypes x (locOf x))
collectPattInfos (PattBinder n) = do
  tn <- freshVar
  return ([(n, VarTypeInfo tn)], tn)
collectPattInfos (PattWildcard rng) = do
  v <- freshVar
  return ([(Name (Text.pack . show $ rng) NoLoc, VarTypeInfo v)], v)
collectPattInfos (PattConstructor n patts) = do
  tPatt           <- freshVar
  tn              <- inferType n
  (infos, tpatts) <- first concat . unzip <$> mapM collectPattInfos patts

  tell [(tn, wrapTFunc tpatts tPatt, n <--> patts)]
  return (infos, tPatt)

instance InferType Op where
  inferType (ChainOp op) = inferType op
  inferType (ArithOp op) = inferType op

instance InferType ChainOp where
  inferType (EQProp  l) = return $ tBool .-> tBool .-> tBool $ l
  inferType (EQPropU l) = return $ tBool .-> tBool .-> tBool $ l
  inferType (EQ      l) = do
    x <- freshVar
    return $ const x .-> const x .-> tBool $ l
  inferType (NEQ  l) = return $ tInt .-> tInt .-> tBool $ l
  inferType (NEQU l) = return $ tInt .-> tInt .-> tBool $ l
  inferType (LTE  l) = return $ tInt .-> tInt .-> tBool $ l
  inferType (LTEU l) = return $ tInt .-> tInt .-> tBool $ l
  inferType (GTE  l) = return $ tInt .-> tInt .-> tBool $ l
  inferType (GTEU l) = return $ tInt .-> tInt .-> tBool $ l
  inferType (LT   l) = do
    x <- freshVar
    return $ const x .-> const x .-> tBool $ l
  inferType (GT l) = do
    x <- freshVar
    return $ const x .-> const x .-> tBool $ l

instance InferType ArithOp where
  inferType (Implies  l) = return $ tBool .-> tBool .-> tBool $ l
  inferType (ImpliesU l) = return $ tBool .-> tBool .-> tBool $ l
  inferType (Conj     l) = return $ tBool .-> tBool .-> tBool $ l
  inferType (ConjU    l) = return $ tBool .-> tBool .-> tBool $ l
  inferType (Disj     l) = return $ tBool .-> tBool .-> tBool $ l
  inferType (DisjU    l) = return $ tBool .-> tBool .-> tBool $ l
  inferType (Neg      l) = return $ tBool .-> tBool $ l
  inferType (NegU     l) = return $ tBool .-> tBool $ l
  inferType (NegNum   l) = return $ tInt .-> tInt $ l
  inferType (Add      l) = return $ tInt .-> tInt .-> tInt $ l
  inferType (Sub      l) = return $ tInt .-> tInt .-> tInt $ l
  inferType (Mul      l) = return $ tInt .-> tInt .-> tInt $ l
  inferType (Div      l) = return $ tInt .-> tInt .-> tInt $ l
  inferType (Mod      l) = return $ tInt .-> tInt .-> tInt $ l
  inferType (Max      l) = return $ tInt .-> tInt .-> tInt $ l
  inferType (Min      l) = return $ tInt .-> tInt .-> tInt $ l
  inferType (Exp      l) = return $ tInt .-> tInt .-> tInt $ l
  inferType (Hash     l) = return $ tBool .-> tInt $ l
  inferType (PointsTo l) = return $ tInt .-> tInt .-> tInt $ l
  inferType (SConj    l) = return $ tBool .-> tBool .-> tBool $ l
  inferType (SImp     l) = return $ tBool .-> tBool .-> tBool $ l

instance InferType Name where
  inferType n = do
    (_, _, s) <- get
    case lookupScopeTree n s of
      Just t  -> return (typeInfoToType t)
      Nothing -> throwError $ NotInScope n

--------------------------------------------------------------------------------
-- unification
type Unifier = (Map Name Type, [Constraint])

emptyUnifier :: Unifier
emptyUnifier = (mempty, [])

unifies :: MonadError TypeError m => Type -> Type -> Loc -> m (Map Name Type)
unifies (TBase t1 _) (TBase t2 _) _ | t1 == t2 = return mempty
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
  | n1 == n2 && length args1 == length args2 = return mempty
unifies (TVar x1 _) (TVar x2 _) _ | x1 == x2 = return mempty
unifies (TVar x _) t@(TBase tb _) _ | x == baseToName tb =
  return $ Map.singleton x t
unifies (TVar x _) t@(TCon n args _) _ | x == n && null args =
  return $ Map.singleton x t
unifies t1 t2@(TVar _ _) l                   = unifies t2 t1 l
unifies (TMetaVar x) (TMetaVar y) _ | x == y = return mempty
unifies (TMetaVar x) t            l          = bind x t l
unifies t            (TMetaVar x) l          = bind x t l
unifies t1           t2           l          = throwError $ UnifyFailed t1 t2 l

bind :: MonadError TypeError m => Name -> Type -> Loc -> m (Map Name Type)
bind x t l | occurs x t = throwError $ RecursiveType x t l
           | otherwise  = return (Map.singleton x t)

solveConstraints :: MonadError TypeError m => [Constraint] -> m (Map Name Type)
solveConstraints cs = solveUnifier (mempty, cs)

solveUnifier :: MonadError TypeError m => Unifier -> m (Map Name Type)
solveUnifier (s, []              ) = return s
solveUnifier (s, (t1, t2, l) : cs) = do
  su <- unifies t1 t2 l
  solveUnifier (su `compose` s, map (f (subst su)) cs)
  where f g (a, b, l') = (g a, g b, l')

--------------------------------------------------------------------------------
-- Type check
type TypeCheckM
  = StateT
      (FreshState, ScopeTreeZipper TypeDefnInfo, ScopeTreeZipper TypeInfo)
      (Except TypeError)

instance Fresh TypeCheckM where
  getCounter = do
    (s, _, _) <- get
    return s
  setCounter s1 = do
    (_, s2, s3) <- get
    put (s1, s2, s3)

instance  HasTypeDefnInfoScope TypeCheckM where
  getTypeDefnInfo = do
    (_, s, _) <- get
    return s
  setTypeDefnInfo s2 = do
    (s1, _, s3) <- get
    put (s1, s2, s3)

instance HasTypeInfoScope TypeCheckM where
  getTypeInfo = do
    (_, _, s) <- get
    return s
  setTypeInfo s3 = do
    (s1, s2, _) <- get
    put (s1, s2, s3)

infer :: InferType a => a -> TypeCheckM Type
infer x = do
  (s, t) <- runInferType x
  updateScopeTreeZipper s
  return t

checkIsType :: InferType a => a -> Type -> TypeCheckM ()
checkIsType x t = do
  (s1, te) <- runInferType x
  s2       <- solveConstraints [(t, te, locOf x)]
  updateScopeTreeZipper (s2 `compose` s1)

updateScopeTreeZipper :: Map Name Type -> TypeCheckM ()
updateScopeTreeZipper s = do
  getTypeInfo >>= setTypeInfo . subst s

runTypeCheck
  :: Program -> Either TypeError (ScopeTreeZipper TypeDefnInfo, ScopeTreeZipper Type)
runTypeCheck prog = do
  let initTypeInfo     = ScopeTreeZipper (ScopeTree mempty mempty) []
  let initTypeDefnInfo = ScopeTreeZipper (ScopeTree mempty mempty) []
  (_, s2, s3) <- runExcept
    (execStateT (typeCheck prog) (0, initTypeDefnInfo, initTypeInfo))
  return (fsRootScopeTree s2, fmap typeInfoToType (fsRootScopeTree s3))

class TypeCheckable a where
    typeCheck :: a -> TypeCheckM ()

instance (TypeCheckable a, Foldable t) => TypeCheckable (t a) where
  typeCheck xs = mapM_ typeCheck xs

class CollectIds a where
    collectIds :: Fresh m => a -> m [(Name, TypeInfo)]

instance CollectIds a => CollectIds [a] where
  collectIds xs = concat <$> mapM collectIds xs

instance CollectIds Definition where
  collectIds (TypeDefn n args ctors _) = do
    let t = TCon n args (n <--> args)
    return $ map
      (\(TypeDefnCtor cn ts) -> (cn, TypeDefnCtorInfo (wrapTFunc ts t)))
      ctors

  collectIds (FuncDefnSig n t _ _) = return [(n, ConstTypeInfo t)]
  collectIds (FuncDefn n _       ) = do
    v <- freshVar
    return [(n, ConstTypeInfo v)]

instance CollectIds Declaration where
  collectIds (ConstDecl ns t _ _) = return $ map (, ConstTypeInfo t) ns
  collectIds (VarDecl   ns t _ _) = return $ map (, VarTypeInfo t) ns

instance TypeCheckable Program where
  typeCheck (Program defns decls exprs stmts loc) = do
    infos <- (<>) <$> collectIds defns <*> collectIds decls
    let tcons = concatMap collectTCon defns
    localScope
      loc
      (tcons, infos)
      (do
        typeCheck defns
        typeCheck decls
        typeCheck exprs
        typeCheck stmts
      )
   where
    collectTCon (TypeDefn n args _ _) = [(n, TypeDefnInfo args)]
    collectTCon _                     = []

instance TypeCheckable Definition where
  typeCheck (TypeDefn _ args ctors _) = do
    let m = Map.fromList (map (, ()) args)
    mapM_ (\(TypeDefnCtor _ ts) -> mapM_ (scopeCheck m) ts) ctors
    typeCheck ctors
   where
    scopeCheck :: MonadError TypeError m => Map Name () -> Type -> m ()
    scopeCheck m (TCon _ args' _) = mapM_
      (\a -> case Map.lookup a m of
        Just _ -> return ()
        _      -> throwError $ NotInScope a
      )
      args'
    scopeCheck _ _ = return ()
  typeCheck (FuncDefnSig _ t prop _) = do
    typeCheck t
    typeCheck prop
  typeCheck (FuncDefn n exprs) = do
    tn <- infer n
    mapM_ (`checkIsType` tn) exprs

instance TypeCheckable TypeDefnCtor where
  typeCheck (TypeDefnCtor _ ts) = typeCheck ts

instance TypeCheckable Declaration where
  typeCheck (ConstDecl _ t prop _) = typeCheck t >> typeCheck prop
  typeCheck (VarDecl   _ t prop _) = typeCheck t >> typeCheck prop

instance TypeCheckable Stmt where
  typeCheck (Skip  _       ) = return ()
  typeCheck (Abort _       ) = return ()
  typeCheck (Assign ns es _) = do
    let ass = zip ns es
    let an  = length ass
    if
      | an < length es -> throwError $ RedundantExprs (drop an es)
      | an < length ns -> throwError $ RedundantNames (drop an ns)
      | otherwise      -> mapM_ checkAssign ass
   where
    checkAssign :: (Name, Expr) -> TypeCheckM ()
    checkAssign (n, expr) = do
      (_, _, s) <- get
      case lookupScopeTree n s of
        Just (VarTypeInfo t) -> checkIsType expr t
        Just _               -> throwError $ AssignToConst n
        Nothing              -> throwError $ NotInScope n
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
    t <- infer x
    mapM_ (`checkIsType` t) es
  typeCheck (HLookup x  e  _) = infer x >>= checkIsType e
  typeCheck (HMutate e1 e2 _) = infer e1 >>= checkIsType e2
  typeCheck (Dispose e _    ) = checkIsType e (tInt NoLoc)
  typeCheck (Block   p _    ) = typeCheck p

instance TypeCheckable GdCmd where
  typeCheck (GdCmd e s _) = checkIsType e (tBool NoLoc) >> typeCheck s

instance TypeCheckable Expr where
  typeCheck = void . infer

instance TypeCheckable Type where
  typeCheck TBase{}            = return ()
  typeCheck (TArray i  t    _) = typeCheck i >> typeCheck t
  typeCheck (TFunc  t1 t2   _) = typeCheck t1 >> typeCheck t2
  typeCheck (TTuple ts) = forM_ ts typeCheck
  typeCheck (TCon   n  args _) = do
    (_, s, _) <- get
    case lookupScopeTree n s of
      Just (TypeDefnInfo args') -> if
        | length args < length args' -> throwError
        $ MissingArguments (drop (length args) args')
        | length args > length args' -> throwError
        $ RedundantNames (drop (length args') args)
        | otherwise -> return ()
      _ -> throwError $ NotInScope n
  typeCheck TVar{}     = return ()
  typeCheck TMetaVar{} = return ()

instance TypeCheckable Interval where
  typeCheck (Interval e1 e2 _) = typeCheck e1 >> typeCheck e2

instance TypeCheckable Endpoint where
  typeCheck (Including e) = typeCheck e
  typeCheck (Excluding e) = typeCheck e

--------------------------------------------------------------------------------
-- helper combinators
typeInfoToType :: TypeInfo -> Type
typeInfoToType (TypeDefnCtorInfo t) = t
typeInfoToType (ConstTypeInfo    t) = t
typeInfoToType (VarTypeInfo      t) = t

freshVar :: Fresh m => m Type
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
arithOpTypes (NegNum   l) = tInt .-> tInt $ l
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
