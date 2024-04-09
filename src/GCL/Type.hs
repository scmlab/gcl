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
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE TypeFamilies #-}

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
import qualified Data.Text                     as Text
import           GCL.Common
import           GHC.Generics                   ( Generic )
import           Prelude                 hiding ( EQ
                                                , GT
                                                , LT
                                                )

import qualified Data.List.NonEmpty            as NonEmpty
import           Server.IntervalMap             ( IntervalMap )
import qualified Server.IntervalMap            as IntervalMap
import           Syntax.Abstract
import           Syntax.Abstract.Util
import           Syntax.Common
import qualified Syntax.Typed                  as Typed

data Index = Index Name | Hole Range deriving (Eq, Show, Ord)

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
  :: (Eq a, MonadError TypeError m) => [(Index, a)] -> m [(Index, a)]
duplicationCheck ns =
  let ds = dups ns
  in  if null ds
        then return ns
        else throwError . DuplicatedIdentifiers . map (toName . fst) . filter isName $ ds
  where
    isName (Index{}, _) = True
    isName _            = False

    toName (Index n) = n
    toName (Hole rng) = Name (Text.pack (show rng)) (locOf rng)

--------------------------------------------------------------------------------
-- Type inference

type TypeEnv = Map Index Type 

class Located a => InferType a where
    inferType :: a -> TypeEnv -> TypeCheckM (Type, Subs Type)

runInferType :: InferType a => a -> TypeCheckM Type
runInferType x = do
  (ty, sub) <- inferType x mempty
  return $ subst sub ty

instance InferType Expr where
  inferType (Lit   lit l) _ = return (litTypes lit l, mempty)
  inferType (Var   x   _) env = inferType x env
  inferType (Const x   _) env = inferType x env
  inferType (Op o       ) env = inferType o env
  inferType (App (App (Op op@(ChainOp _)) e1 _) e2 l) env =  undefined {- do
    top <- inferType op env

    (t1, s1)  <- case e1 of
      App (App (Op (ChainOp _)) _ _) e12 _ -> do
        _ <- inferType e1 env
        inferType e12 env
      _ -> inferType e1 env

    (t2, s2) <- inferType e2 env
    v  <- freshVar
    tell [(top, t1 ~-> t2 ~-> v, l)]

    return (tBool l, mempty) -}
  inferType (App e1 e2 loc) env = do
    v <- freshVar
    (t1, s1) <- inferType e1 env
    (t2, s2) <- inferType e2 (subst s1 env)
    s3 <- unifies (subst s2 t1) (TFunc t2 v NoLoc) loc
    return (v, s3 `compose` s2 `compose` s1)
  inferType (Lam bound expr loc) env = do
    tv <- freshVar
    let newEnv = Map.insert (Index bound) tv env
    (t1, s1) <- inferType expr newEnv
    return (TFunc (subst s1 tv) t1 loc, s1)
  inferType _ _ = undefined -- FIXME:

instance InferType Op where
  inferType (ChainOp op) = inferType op
  inferType (ArithOp op) = inferType op

instance InferType ChainOp where
  inferType :: ChainOp -> TypeEnv -> TypeCheckM (Type, Subs Type)
  inferType (EQProp  l) env = return (tBool .-> tBool .-> tBool $ l, mempty)
  inferType (EQPropU l) env = return (tBool .-> tBool .-> tBool $ l, mempty)
  inferType (EQ      l) env = do
    x <- freshVar
    return (const x .-> const x .-> tBool $ l, mempty)
  inferType (NEQ  l) env = return (tInt .-> tInt .-> tBool $ l, mempty)
  inferType (NEQU l) env = return (tInt .-> tInt .-> tBool $ l, mempty)
  inferType (LTE  l) env = return (tInt .-> tInt .-> tBool $ l, mempty)
  inferType (LTEU l) env = return (tInt .-> tInt .-> tBool $ l, mempty)
  inferType (GTE  l) env = return (tInt .-> tInt .-> tBool $ l, mempty)
  inferType (GTEU l) env = return (tInt .-> tInt .-> tBool $ l, mempty)
  inferType (LT   l) env = do
    x <- freshVar
    return (const x .-> const x .-> tBool $ l, mempty)
  inferType (GT l) env = do
    x <- freshVar
    return (const x .-> const x .-> tBool $ l, mempty)

instance InferType ArithOp where
  inferType (Implies  l) _ = return (tBool .-> tBool .-> tBool $ l, mempty)
  inferType (ImpliesU l) _ = return (tBool .-> tBool .-> tBool $ l, mempty)
  inferType (Conj     l) _ = return (tBool .-> tBool .-> tBool $ l, mempty)
  inferType (ConjU    l) _ = return (tBool .-> tBool .-> tBool $ l, mempty)
  inferType (Disj     l) _ = return (tBool .-> tBool .-> tBool $ l, mempty)
  inferType (DisjU    l) _ = return (tBool .-> tBool .-> tBool $ l, mempty)
  inferType (Neg      l) _ = return (tBool .-> tBool $ l, mempty)
  inferType (NegU     l) _ = return (tBool .-> tBool $ l, mempty)
  inferType (NegNum   l) _ = return (tInt .-> tInt $ l, mempty)
  inferType (Add      l) _ = return (tInt .-> tInt .-> tInt $ l, mempty)
  inferType (Sub      l) _ = return (tInt .-> tInt .-> tInt $ l, mempty)
  inferType (Mul      l) _ = return (tInt .-> tInt .-> tInt $ l, mempty)
  inferType (Div      l) _ = return (tInt .-> tInt .-> tInt $ l, mempty)
  inferType (Mod      l) _ = return (tInt .-> tInt .-> tInt $ l, mempty)
  inferType (Max      l) _ = return (tInt .-> tInt .-> tInt $ l, mempty)
  inferType (Min      l) _ = return (tInt .-> tInt .-> tInt $ l, mempty)
  inferType (Exp      l) _ = return (tInt .-> tInt .-> tInt $ l, mempty)
  inferType (Hash     l) _ = return (tBool .-> tInt $ l, mempty)
  inferType (PointsTo l) _ = return (tInt .-> tInt .-> tInt $ l, mempty)
  inferType (SConj    l) _ = return (tBool .-> tBool .-> tBool $ l, mempty)
  inferType (SImp     l) _ = return (tBool .-> tBool .-> tBool $ l, mempty)

instance InferType Name where
  inferType n env = do
    case Map.lookup (Index n) env of
      Just t  -> return (t, mempty)
      Nothing -> throwError $ NotInScope n -- FIXME: Errors are thrown here.

--------------------------------------------------------------------------------
-- unification
type Unifier = (Map Name Type, Subs Type)

emptyUnifier :: Unifier
emptyUnifier = (mempty, mempty)

unifies :: MonadError TypeError m => Type -> Type -> Loc -> m (Subs Type)
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

--------------------------------------------------------------------------------
-- Type check

type family Typed untyped where
  Typed Definition = Typed.TypedDefinition
  Typed Declaration = Typed.TypedDeclaration
  Typed TypeDefnCtor = Typed.TypedTypeDefnCtor
  Typed Program = Typed.TypedProgram
  Typed Stmt = Typed.TypedStmt
  Typed GdCmd = Typed.TypedGdCmd
  Typed Expr = Typed.TypedExpr
  Typed Name = Maybe TypeInfo
  Typed Type = ()
  Typed Interval = ()
  Typed Endpoint = ()
  Typed [a] = [Typed a]
  Typed (Maybe a) = Maybe (Typed a)

type TypeCheckM = StateT (FreshState, Map Index TypeDefnInfo, Map Index TypeInfo) (Except TypeError)

instance Counterous TypeCheckM where
  countUp = do
    (count, typeDefnInfo, typeInfo) <- get
    put (succ count, typeDefnInfo, typeInfo)
    return count

checkIsType :: InferType a => a -> Type -> TypeCheckM () -- FIXME: Loc

checkIsType x expected = do
  (_, _, info) <- get
  (actual, _) <- inferType x $ typeInfoToType <$> info
  sub <- unifies actual expected NoLoc
  if Map.null sub -- FIXME: Either `==` doesn't work here or something else is wrong.
    then return ()
    else throwError $ UnifyFailed actual expected NoLoc

runTypeCheck
  :: Program -> Either TypeError Typed.TypedProgram
runTypeCheck prog = do
  (program, _state) <- runExcept (runStateT (typeCheck prog) (0, mempty, mempty))
  Right program

class TypeCheckable a where
    typeCheck :: a -> TypeCheckM (Typed a)

class CollectIds a where
    collectIds :: Fresh m => a -> m [(Index, TypeInfo)]

instance CollectIds a => CollectIds [a] where
  collectIds xs = concat <$> mapM collectIds xs

instance CollectIds Definition where
  collectIds (TypeDefn n args ctors _) = do
    let t = TCon n args (n <--> args)
    return $ map
      (\(TypeDefnCtor cn ts) -> (Index cn, TypeDefnCtorInfo (wrapTFunc ts t)))
      ctors

  collectIds (FuncDefnSig n t _ _) = return [(Index n, ConstTypeInfo t)]
  collectIds (FuncDefn n _       ) = do
    v <- freshVar
    return [(Index n, ConstTypeInfo v)]

instance CollectIds Declaration where
  collectIds (ConstDecl ns t _ _) = return $ map ((, ConstTypeInfo t) . Index) ns
  collectIds (VarDecl   ns t _ _) = return $ map ((, VarTypeInfo t) . Index) ns

instance TypeCheckable Program where
  typeCheck (Program defns decls exprs stmts loc) = do
    infos <- (<>) <$> collectIds defns <*> collectIds decls
    modify (\(freshState, typeDefnInfos, origInfos) -> (freshState, typeDefnInfos, Map.union origInfos $ Map.fromList infos))
    let tcons = concatMap collectTCon defns
    modify (\(freshState, origInfos, typeInfos) -> (freshState, Map.union origInfos $ Map.fromList tcons, typeInfos))
    typedDefns <- mapM typeCheck defns
    typedDecls <- mapM typeCheck decls
    typedExprs <- mapM typeCheck exprs
    typedStmts <- mapM typeCheck stmts
    return $ Typed.Program typedDefns typedDecls [] typedStmts loc -- FIXME:
   where
    collectTCon (TypeDefn n args _ _) = [(Index n, TypeDefnInfo args)]
    collectTCon _                     = []

instance TypeCheckable Definition where
  typeCheck (TypeDefn name args ctors loc) = do
    let m = Map.fromList (map (, ()) args)
    mapM_ (\(TypeDefnCtor _ ts) -> mapM_ (scopeCheck m) ts) ctors
    ctors' <- mapM typeCheck ctors
    return $ Typed.TypeDefn name args ctors' loc
   where
    scopeCheck :: MonadError TypeError m => Map Name () -> Type -> m ()
    scopeCheck m (TCon _ args' _) = mapM_
      (\a -> case Map.lookup a m of
        Just _ -> return ()
        _      -> throwError $ NotInScope a
      )
      args'
    scopeCheck _ _ = return ()
  typeCheck (FuncDefnSig name ty expr loc) = do
    _ <- typeCheck ty
    expr' <- mapM typeCheck expr
    return $ Typed.FuncDefnSig name ty expr' loc
  typeCheck (FuncDefn name exprs) = do
    (_, _, info) <- get
    (tn, sn) <- inferType name $ typeInfoToType <$> info -- FIXME: This part might be wrong. (Edit: it's wrong)
    mapM_ (`checkIsType` tn) exprs
    exprs' <- mapM typeCheck exprs
    return $ Typed.FuncDefn name exprs'

instance TypeCheckable TypeDefnCtor where
  typeCheck (TypeDefnCtor name ts) = do -- TODO: I deleted some code which might be purposeful
    return $ Typed.TypedTypeDefnCtor name ts

instance TypeCheckable Declaration where
  typeCheck (ConstDecl names ty prop loc) =
    case prop of
      Just p -> do
        typeCheck ty
        p' <- typeCheck p
        return $ Typed.ConstDecl names ty (Just p') loc
      Nothing -> return $ Typed.ConstDecl names ty Nothing loc
  typeCheck (VarDecl names ty prop loc) =
    case prop of
      Just p -> do
        typeCheck ty
        p' <- typeCheck p
        return $ Typed.VarDecl names ty (Just p') loc
      Nothing -> return $ Typed.VarDecl names ty Nothing loc

typeOf :: Typed.TypedExpr -> Type
typeOf (Typed.Lit _ ty _) = ty
typeOf (Typed.Var _ ty _) = ty
typeOf (Typed.Const _ ty _) = ty
typeOf _ = undefined -- FIXME:

instance TypeCheckable Stmt where
  typeCheck (Skip  loc     ) = return $ Typed.Skip loc
  typeCheck (Abort loc     ) = return $ Typed.Abort loc
  typeCheck (Assign names exprs loc) = do
    let ass = zip names exprs
    let an  = length ass
    if
      | an < length exprs -> throwError $ RedundantExprs (drop an exprs)
      | an < length names -> throwError $ RedundantNames (drop an names)
      | otherwise      -> do
        (_, _, infos) <- get
        mapM_ (checkAssign infos) ass
        exprs' <- mapM typeCheck exprs
        return $ Typed.Assign names exprs' loc
   where
    checkAssign :: Map Index TypeInfo -> (Name, Expr) -> TypeCheckM ()
    checkAssign infos (name, expr) = do
      case Map.lookup (Index name) infos of
        Just (VarTypeInfo t) -> checkIsType expr t >> pure ()
        Just _               -> throwError $ AssignToConst name
        Nothing              -> throwError $ NotInScope name
  typeCheck (AAssign arr index e loc) = do
    typedIndex <- typeCheck index
    if typeOf typedIndex == tInt NoLoc
    then do
      (_, _, info) <- get
      (te, _) <- inferType e $ typeInfoToType <$> info
      typedArr <- typeCheck arr
      if typeOf typedArr == TArray (Interval (Including index) (Including index) (locOf index)) te (locOf arr)
      then do
        e' <- typeCheck e
        return $ Typed.AAssign typedArr typedIndex e' loc
      else throwError $ UnifyFailed (typeOf typedArr) (TArray (Interval (Including index) (Including index) (locOf index)) te (locOf arr)) loc
    else throwError $ UnifyFailed (typeOf typedIndex) (tInt NoLoc) loc
  typeCheck (Assert expr loc        ) = do
    typedExpr <- typeCheck expr
    if typeOf typedExpr == tBool NoLoc
    then return (Typed.Assert typedExpr loc)
    else throwError $ UnifyFailed (typeOf typedExpr) (tBool NoLoc) loc
  typeCheck (LoopInvariant e1 e2 loc) = do
    e1' <- typeCheck e1
    if typeOf e1' == tBool NoLoc
    then do
      e2' <- typeCheck e2
      if typeOf e2' == tInt NoLoc
      then return $ Typed.LoopInvariant e1' e2' loc
      else throwError $ UnifyFailed (typeOf e2') (tBool NoLoc) loc
    else throwError $ UnifyFailed (typeOf e1') (tBool NoLoc) loc
  typeCheck (Do gds loc) = do
    gds' <- mapM typeCheck gds
    return $ Typed.Do gds' loc
  typeCheck (If gds loc) = do
    gds' <-  mapM typeCheck gds
    return $ Typed.If gds' loc
  typeCheck (Spec text range) = return $ Typed.Spec text range
  typeCheck (Proof text1 text2 range) = return $ Typed.Proof text1 text2 range
  typeCheck _ = undefined -- FIXME:

instance TypeCheckable GdCmd where
  typeCheck (GdCmd e s loc) = do
    e' <- typeCheck e
    if typeOf e' == tBool NoLoc
    then do
      s' <- mapM typeCheck s
      return $ Typed.TypedGdCmd e' s' loc
    else throwError $ UnifyFailed (typeOf e') (tBool NoLoc) loc

instance TypeCheckable Expr where
  typeCheck expr = case expr of
    Lit lit loc -> do
      let litTy = litTypes lit loc
      return $ Typed.Lit lit litTy loc
    Var name loc -> do
      (_, _, info) <- get
      (nameTy, _) <- inferType name $ typeInfoToType <$> info
      return $ Typed.Var name nameTy loc
    Const name loc -> do
      (_, _, info) <- get
      (nameTy, _) <- inferType name $ typeInfoToType <$> info
      return $ Typed.Const name nameTy loc
    Op op -> do
      (_, _, info) <- get
      (opTy, _) <- inferType op $ typeInfoToType <$> info
      return $ Typed.Op op opTy
    App expr1 expr2 loc -> do
      typedExpr1 <- typeCheck expr1
      typedExpr2 <- typeCheck expr2
      -- FIXME: Add check
      return $ Typed.App typedExpr1 typedExpr2 loc
    Lam name expr loc -> do
      (_, _, info) <- get
      (nameTy, _) <- inferType name $ typeInfoToType <$> info -- FIXME: This is probably wrong.
      typedExpr <- typeCheck expr
      return $ Typed.Lam name nameTy typedExpr loc
    Func na ne loc -> undefined -- FIXME: This and below.
    Tuple exs -> undefined
    Quant ex nas ex' ex3 loc -> undefined
    RedexKernel na ex set ne -> undefined
    RedexShell n ex -> undefined
    ArrIdx ex ex' loc -> undefined
    ArrUpd ex ex' ex3 loc -> undefined
    Case ex ccs loc -> undefined

instance TypeCheckable Type where
  typeCheck TBase{} = return ()
  typeCheck (TArray i t _) = do
    typeCheck i
    typeCheck t
    return ()
  typeCheck (TFunc  t1 t2 _) = typeCheck t1 >> typeCheck t2
  typeCheck (TTuple ts     )  = forM_ ts typeCheck
  typeCheck (TCon name args _ ) = do
    (_, infos, _) <- get
    case Map.lookup (Index name) infos of -- TODO: Check if this is right.
      Just ty -> undefined -- FIXME:
      {-if 
        | length args < length args' -> throwError
        $ MissingArguments (drop (length args) args')
        | length args > length args' -> throwError
        $ RedundantNames (drop (length args') args)
        | otherwise -> return () -}
      _ -> throwError $ NotInScope name
  typeCheck TVar{} = return ()
  typeCheck TMetaVar{} = return ()

instance TypeCheckable Interval where
  typeCheck (Interval e1 e2 _) = typeCheck e1 >> typeCheck e2

instance TypeCheckable Endpoint where
  typeCheck (Including e) = return ()
  typeCheck (Excluding e) = return () -- FIXME: This might be wrong

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

-- A class for substitution not needing a Fresh monad.
-- Used only in this module.
-- Moved from GCL.Common to here.
--  SCM: think about integrating it with the other substitution.

class Substitutable a b where
  subst :: Subs a -> b -> b

compose :: Substitutable a a => Subs a -> Subs a -> Subs a
s1 `compose` s2 = s1 <> Map.map (subst s1) s2

instance (Substitutable a b, Functor f) => Substitutable a (f b) where
  subst = fmap . subst

instance Substitutable Type Type where
  subst _ t@TBase{}       = t
  subst s (TArray i t l ) = TArray i (subst s t) l
  subst s (TTuple ts    ) = TTuple (map (subst s) ts)
  subst s (TFunc t1 t2 l) = TFunc (subst s t1) (subst s t2) l
  subst _ t@TCon{}        = t
  subst _ t@TVar{}        = t
  subst s t@(TMetaVar n)  = Map.findWithDefault t n s
