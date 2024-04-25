{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE TypeFamilies #-}

module GCL.Type where

import           Control.Monad.Except
import           Control.Monad.State.Lazy
import           Control.Monad.Writer.Lazy
import           Data.Aeson                     ( ToJSON )
import           Data.Bifunctor                 ( Bifunctor (second) )
import           Data.Functor
import           Data.Maybe                     ( fromJust )
import           Data.List
import           Data.Loc                       ( (<-->)
                                                , Loc(..)
                                                , Located
                                                , locOf
                                                )
import           Data.Loc.Range                 ( Range )
import qualified Data.Map                      as Map
import qualified Data.Text                     as Text
import           GCL.Common
import           GHC.Generics                   ( Generic )
import           Prelude                 hiding ( EQ
                                                , GT
                                                , LT
                                                )

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
-- The elaboration monad

type ElabaratorM = StateT (FreshState, [(Index, TypeDefnInfo)], [(Index, TypeInfo)]) (Except TypeError)

instance Counterous ElabaratorM where
  countUp = do
    (count, typeDefnInfo, typeInfo) <- get
    put (succ count, typeDefnInfo, typeInfo)
    return count

runElaboration
  :: Program -> Either TypeError Typed.TypedProgram
runElaboration prog = do
  ((_, program, _), _state) <- runExcept (runStateT (elaborate prog mempty) (0, mempty, mempty))
  Right program

--------------------------------------------------------------------------------
-- Collecting ids

instance CollectIds Definition where
  collectIds (TypeDefn name args ctors _) = do
    modify (\(freshState, origTypeDefnInfos, origTypeInfos) -> (freshState, newTypeDefnInfos : origTypeDefnInfos, newTypeInfos <> origTypeInfos))
    where
      newTypeDefnInfos = (Index name, TypeDefnInfo args)
      newTypeInfos =
        map
        (\(TypeDefnCtor cn ts) -> (Index cn, TypeDefnCtorInfo (wrapTFunc ts (TCon name args (name <--> args)))))
        ctors

  collectIds (FuncDefnSig n t _ _) = modify (\(freshState, typeDefnInfos, origInfos) -> (freshState, typeDefnInfos, infos <> origInfos))
    where infos = [(Index n, ConstTypeInfo t)]
  
  collectIds (FuncDefn name exprs) = do
    (_, _, infos) <- get
    case lookup (Index name) $ Data.Bifunctor.second typeInfoToType <$> infos of
      Just ty -> mapM_ (\expr -> do
                        (exprTy, _, exprSub) <- elaborate expr $ Data.Bifunctor.second typeInfoToType <$> infos
                        unifies (subst exprSub $ fromJust exprTy) ty (locOf expr)
                       ) exprs
      Nothing -> do
        inferred <- mapM (`elaborate` (Data.Bifunctor.second typeInfoToType <$> infos)) exprs
        let infos' = (\(ty, _, sub) -> (Index name, ConstTypeInfo $ subst sub $ fromJust ty)) <$> inferred
        modify (\(freshState, typeDefnInfos, origInfos) -> (freshState, typeDefnInfos, infos' <> origInfos))
        return ()

instance CollectIds Declaration where
  collectIds (ConstDecl ns t _ _) = modify (\(freshState, typeDefnInfos, origInfos) -> (freshState, typeDefnInfos, infos <> origInfos))
    where infos = map ((, ConstTypeInfo t) . Index) ns
  collectIds (VarDecl   ns t _ _) = modify (\(freshState, typeDefnInfos, origInfos) -> (freshState, typeDefnInfos, infos <> origInfos))
    where infos = map ((, VarTypeInfo t) . Index) ns


--------------------------------------------------------------------------------
-- Elaboration

type TypeEnv = [(Index, Type)]

type family Typed untyped where
  Typed Definition = Typed.TypedDefinition
  Typed Declaration = Typed.TypedDeclaration
  Typed TypeDefnCtor = Typed.TypedTypeDefnCtor
  Typed Program = Typed.TypedProgram
  Typed Stmt = Typed.TypedStmt
  Typed GdCmd = Typed.TypedGdCmd
  Typed Expr = Typed.TypedExpr
  Typed Name = Name
  Typed Op = Op
  Typed ChainOp = Op
  Typed ArithOp = Op
  Typed Type = ()
  Typed Interval = ()
  Typed Endpoint = ()
  Typed [a] = [Typed a]
  Typed (Maybe a) = Maybe (Typed a)

class Located a => Elab a where
    elaborate :: a -> TypeEnv -> ElabaratorM (Maybe Type, Typed a, Subs Type)

instance Elab Program where
  elaborate (Program defns decls exprs stmts loc) _env = do
    collectIds decls
    collectIds $ reverse defns
    let tcons = concatMap collectTCon defns
    modify (\(freshState, origInfos, typeInfos) -> (freshState, tcons <> origInfos, typeInfos))
    (_, _, infos) <- get
    typedDefns <- mapM (\defn -> do
                          typedDefn <- elaborate defn $ Data.Bifunctor.second typeInfoToType <$> infos
                          let (_, typed, _) = typedDefn
                          return typed
                       ) defns
    typedDecls <- mapM (\decl -> do
                          typedDecl <- elaborate decl $ Data.Bifunctor.second typeInfoToType <$> infos
                          let (_, typed, _) = typedDecl
                          return typed
                       ) decls
    typedExprs <- mapM (\expr -> do
                          typedExpr <- elaborate expr $ Data.Bifunctor.second typeInfoToType <$> infos
                          let (_, typed, _) = typedExpr
                          return typed
                       ) exprs
    typedStmts <- mapM (\stmt -> do
                          typedStmt <- elaborate stmt $ Data.Bifunctor.second typeInfoToType <$> infos
                          let (_, typed, _) = typedStmt
                          return typed
                       ) stmts
    return (Nothing, Typed.Program typedDefns typedDecls typedExprs typedStmts loc, mempty)
   where
    collectTCon (TypeDefn n args _ _) = [(Index n, TypeDefnInfo args)]
    collectTCon _                     = []

instance Elab Definition where
  elaborate (TypeDefn name args ctors loc) env = do
    let m = Map.fromList (map (, ()) args)
    mapM_ (\(TypeDefnCtor _ ts) -> mapM_ (scopeCheck m) ts) ctors
    ctors' <- mapM (\ctor -> do
                      typedCtor <- elaborate ctor env
                      let (_, typed, _) = typedCtor
                      return typed
                   ) ctors
    return (Nothing, Typed.TypeDefn name args ctors' loc, mempty)
   where
    scopeCheck :: MonadError TypeError m => Map.Map Name () -> Type -> m ()
    scopeCheck m (TCon _ args' _) = mapM_
      (\a -> case Map.lookup a m of
        Just _ -> return ()
        _      -> throwError $ NotInScope a
      )
      args'
    scopeCheck _ _ = return ()
  elaborate (FuncDefnSig name ty maybeExpr loc) env = do
    expr' <- mapM (\expr -> do
                    typedExpr <- elaborate expr env
                    let (_, typed, _) = typedExpr
                    return typed
                  ) maybeExpr
    return (Nothing, Typed.FuncDefnSig name ty expr' loc, mempty)
  elaborate (FuncDefn name exprs) env = do
    exprs' <- mapM (\expr -> do
                      typedExpr <- elaborate expr env
                      let (_, typed, _) = typedExpr
                      return typed
                   ) exprs
    return (Nothing, Typed.FuncDefn name exprs', mempty)

instance Elab TypeDefnCtor where
  elaborate (TypeDefnCtor name ts) _ = do
    return (Nothing, Typed.TypedTypeDefnCtor name ts, mempty)

instance Elab Declaration where
  elaborate (ConstDecl names ty prop loc) env =
    case prop of
      Just p -> do
        (_, p', _) <- elaborate p env
        return (Nothing, Typed.ConstDecl names ty (Just p') loc, mempty)
      Nothing -> return (Nothing, Typed.ConstDecl names ty Nothing loc, mempty)
  elaborate (VarDecl names ty prop loc) env =
    case prop of
      Just p -> do
        (_, p', _) <- elaborate p env
        return (Nothing, Typed.VarDecl names ty (Just p') loc, mempty)
      Nothing -> return (Nothing, Typed.VarDecl names ty Nothing loc, mempty)

instance Elab Stmt where
  elaborate (Skip  loc     ) _ = return (Nothing, Typed.Skip loc, mempty)
  elaborate (Abort loc     ) _ = return (Nothing, Typed.Abort loc, mempty)
  elaborate (Assign names exprs loc) env = do
    let ass = zip names exprs
    let an  = length ass
    if
      | an < length exprs -> throwError $ RedundantExprs (drop an exprs)
      | an < length names -> throwError $ RedundantNames (drop an names)
      | otherwise      -> do
        (_, _, infos) <- get
        mapM_ (checkAssign infos) ass
        exprs' <- mapM (\expr -> do
                          typedExpr <- elaborate expr env
                          let (_, typed, _) = typedExpr
                          return typed
                       ) exprs
        return (Nothing, Typed.Assign names exprs' loc, mempty)
   where
    checkAssign :: [(Index, TypeInfo)] -> (Name, Expr) -> ElabaratorM ()
    checkAssign infos (name, _expr) = do
      case lookup (Index name) infos of
        Just (VarTypeInfo _) -> pure ()
        Just _               -> throwError $ AssignToConst name
        Nothing              -> throwError $ NotInScope name
  elaborate (AAssign arr index e loc) _ = undefined {- do -- TODO:
    checkIsType index $ tInt NoLoc
    typedIndex <- typeCheck index
    (_, _, infos) <- get
    (te, _) <- elaborate e $ Data.Bifunctor.second typeInfoToType <$> infos
    checkIsType arr $ TArray (Interval (Including index) (Including index) (locOf index)) te (locOf arr)
    typedArr <- typeCheck arr
    e' <- typeCheck e
    return $ Typed.AAssign typedArr typedIndex e' loc -}
  elaborate (Assert expr loc        ) env = do
    (ty, _, sub) <- elaborate expr env
    _ <- unifies (subst sub $ fromJust ty) (tBool NoLoc) loc
    (_, typedExpr, _) <- elaborate expr env
    return (Nothing, Typed.Assert typedExpr loc, mempty)
  elaborate (LoopInvariant e1 e2 loc) env = do
    (ty1, _, sub1) <- elaborate e1 env
    _ <- unifies (subst sub1 $ fromJust ty1) (tBool NoLoc) loc
    (_, e1', _) <- elaborate e1 env
    (ty2, _, sub2) <- elaborate e2 env
    _ <- unifies (subst sub2 $ fromJust ty2) (tInt NoLoc) loc
    (_, e2', _) <- elaborate e2 env
    return (Nothing, Typed.LoopInvariant e1' e2' loc, mempty)
  elaborate (Do gds loc) env = do
    gds' <- mapM (\gd -> do
                  typedGd <- elaborate gd env
                  let (_, typed, _) = typedGd
                  return typed
                 ) gds
    return (Nothing, Typed.Do gds' loc, mempty)
  elaborate (If gds loc) env = do
    gds' <- mapM (\gd -> do
                  typedGd <- elaborate gd env
                  let (_, typed, _) = typedGd
                  return typed
                 ) gds
    return (Nothing, Typed.If gds' loc, mempty)
  elaborate (Spec text range) _ = return (Nothing, Typed.Spec text range, mempty)
  elaborate (Proof text1 text2 range) _ = return (Nothing, Typed.Proof text1 text2 range, mempty)
  elaborate _ _ = undefined -- FIXME:

instance Elab GdCmd where
  elaborate (GdCmd expr stmts loc) env = do
    (ty, _, sub) <- elaborate expr env
    _ <- unifies (subst sub $ fromJust ty) (tBool NoLoc) loc
    (_, e', _) <- elaborate expr env
    s' <- mapM (\stmt -> do
                  typedStmt <- elaborate stmt env
                  let (_, typed, _) = typedStmt
                  return typed
               ) stmts
    return (Nothing, Typed.TypedGdCmd e' s' loc, mempty)

instance Elab Expr where
  elaborate (Lit lit loc) _ = let ty = litTypes lit loc in return (Just ty, Typed.Lit lit ty loc, mempty)
  elaborate (Var x loc) env = case lookup (Index x) env of
      Just ty -> return (Just ty, Typed.Var x ty loc, mempty)
      Nothing -> throwError $ NotInScope x
  elaborate (Const x loc) env = case lookup (Index x) env of
      Just ty -> return (Just ty, Typed.Const x ty loc, mempty)
      Nothing -> throwError $ NotInScope x
  elaborate (Op o) env = (\(ty, op, sub) -> (ty, Typed.Op op $ fromJust ty, sub)) <$> elaborate o env
  {- elaborate (App (App (Op op@(ChainOp _)) e1 _) e2 l) env = do -- FIXME: Make chain operators work.
    top <- elaborate op env

    (t1, s1)  <- case e1 of
      App (App (Op (ChainOp _)) _ _) e12 _ -> do
        _ <- elaborate e1 env
        elaborate e12 env
      _ -> elaborate e1 env

    (t2, s2) <- elaborate e2 env
    v  <- freshVar
    tell [(top, t1 ~-> t2 ~-> v, l)]

    return (tBool l, mempty) -}
  elaborate (App e1 e2 loc) env = do
    tv <- freshVar
    (ty1, typedExpr1, sub1) <- elaborate e1 env
    (ty2, typedExpr2, sub2) <- elaborate e2 (subst sub1 env)
    sub3 <- unifies (subst sub2 $ fromJust ty1) (TFunc (fromJust ty2) tv NoLoc) loc
    return (Just $ subst sub3 tv, Typed.App typedExpr1 typedExpr2 loc, sub3 `compose` sub2 `compose` sub1)
  elaborate (Lam bound expr loc) env = do
    tv <- freshVar
    let newEnv = (Index bound, tv) : env
    (ty1, typedExpr1, sub1) <- elaborate expr newEnv
    let returnTy = TFunc (subst sub1 tv) (fromJust ty1) loc
    return (Just returnTy, Typed.Lam bound (subst sub1 tv) typedExpr1 loc, sub1)
  elaborate (Func name clauses l) env = undefined -- TODO: Implement below cases for type checking exprs.
  elaborate (Tuple xs) env = undefined
  elaborate (Quant quantifier bound restriction inner loc) env = do -- TODO: implement `#` as a quantifer.
    tv <- freshVar
    (quantTy, quantTypedExpr, quantSub) <- elaborate quantifier env
    uniSub <- unifies (subst quantSub $ fromJust quantTy) (tv ~-> tv ~-> tv) (locOf quantifier)
    tvs <- replicateM (length bound) freshVar
    let newEnv = zip (Index <$> bound) tvs <> subst (quantSub `compose` uniSub) env
    (resTy, resTypedExpr, resSub) <- elaborate restriction newEnv
    uniSub2 <- unifies (subst resSub $ fromJust resTy) (tBool NoLoc) (locOf restriction)
    let newEnv' = subst (resSub `compose` uniSub2) newEnv
    (innerTy, innerTypedExpr, innerSub) <- elaborate inner newEnv'
    uniSub3 <- unifies (subst innerSub $ fromJust innerTy) tv (locOf inner)
    return (Just $ subst quantSub tv, Typed.Quant quantTypedExpr bound resTypedExpr innerTypedExpr loc, quantSub `compose` resSub `compose` innerSub `compose` uniSub `compose` uniSub2 `compose` uniSub3)
  elaborate (RedexShell _ expr) env = undefined
  elaborate (RedexKernel n _ _ _) env = undefined
  elaborate (ArrIdx e1 e2 loc) env = do
    tv <- freshVar
    (ty1, typedExpr1, sub1) <- elaborate e1 env
    (ty2, typedExpr2, sub2) <- elaborate e2 (subst sub1 env)
    sub3 <- unifies (subst sub2 $ fromJust ty2) (tInt NoLoc) (locOf e2)
    sub4 <- unifies (subst (sub2 `compose` sub3) (fromJust ty1)) (TFunc (tInt NoLoc) tv NoLoc) loc
    return (Just $ subst sub3 tv, Typed.ArrIdx typedExpr1 typedExpr2 loc, sub4 `compose` sub3 `compose` sub2 `compose` sub1)
  elaborate (ArrUpd e1 e2 e3 _) env = elaborate e3 env -- TODO:
  elaborate (Case expr cs l) env = undefined

instance Elab Op where
  elaborate (ChainOp op) = elaborate op
  elaborate (ArithOp op) = elaborate op

instance Elab ChainOp where
  elaborate (EQProp  l) _ = return (Just $ tBool .-> tBool .-> tBool $ l, ChainOp $ EQProp l, mempty)
  elaborate (EQPropU l) _ = return (Just $ tBool .-> tBool .-> tBool $ l, ChainOp $ EQPropU l, mempty)
  elaborate (EQ      l) _ = do
    x <- freshVar
    return (Just $ const x .-> const x .-> tBool $ l, ChainOp $ EQ l, mempty)
  elaborate (NEQ  l) _ = return (Just $ tInt .-> tInt .-> tBool $ l, ChainOp $ NEQ l, mempty)
  elaborate (NEQU l) _ = return (Just $ tInt .-> tInt .-> tBool $ l, ChainOp $ NEQU l, mempty)
  elaborate (LTE  l) _ = return (Just $ tInt .-> tInt .-> tBool $ l, ChainOp $ LTE l, mempty)
  elaborate (LTEU l) _ = return (Just $ tInt .-> tInt .-> tBool $ l, ChainOp $ LTEU l, mempty)
  elaborate (GTE  l) _ = return (Just $ tInt .-> tInt .-> tBool $ l, ChainOp $ GTE l, mempty)
  elaborate (GTEU l) _ = return (Just $ tInt .-> tInt .-> tBool $ l, ChainOp $ GTEU l, mempty)
  elaborate (LT   l) _ = do
    x <- freshVar
    return (Just $ const x .-> const x .-> tBool $ l, ChainOp $ LT l, mempty)
  elaborate (GT l) _ = do
    x <- freshVar
    return (Just $ const x .-> const x .-> tBool $ l, ChainOp $ GT l, mempty)

instance Elab ArithOp where
  elaborate (Implies  l) _ = return (Just $ tBool .-> tBool .-> tBool $ l, ArithOp $ Implies l, mempty)
  elaborate (ImpliesU l) _ = return (Just $ tBool .-> tBool .-> tBool $ l, ArithOp $ ImpliesU l, mempty)
  elaborate (Conj     l) _ = return (Just $ tBool .-> tBool .-> tBool $ l, ArithOp $ Conj l, mempty)
  elaborate (ConjU    l) _ = return (Just $ tBool .-> tBool .-> tBool $ l, ArithOp $ ConjU l, mempty)
  elaborate (Disj     l) _ = return (Just $ tBool .-> tBool .-> tBool $ l, ArithOp $ Disj l, mempty)
  elaborate (DisjU    l) _ = return (Just $ tBool .-> tBool .-> tBool $ l, ArithOp $ DisjU l, mempty)
  elaborate (Neg      l) _ = return (Just $ tBool .-> tBool $ l, ArithOp $ Neg l, mempty)
  elaborate (NegU     l) _ = return (Just $ tBool .-> tBool $ l, ArithOp $ NegU l, mempty)
  elaborate (NegNum   l) _ = return (Just $ tInt .-> tInt $ l, ArithOp $ NegNum l, mempty)
  elaborate (Add      l) _ = return (Just $ tInt .-> tInt .-> tInt $ l, ArithOp $ Add l, mempty)
  elaborate (Sub      l) _ = return (Just $ tInt .-> tInt .-> tInt $ l, ArithOp $ Sub l, mempty)
  elaborate (Mul      l) _ = return (Just $ tInt .-> tInt .-> tInt $ l, ArithOp $ Mul l, mempty)
  elaborate (Div      l) _ = return (Just $ tInt .-> tInt .-> tInt $ l, ArithOp $ Div l, mempty)
  elaborate (Mod      l) _ = return (Just $ tInt .-> tInt .-> tInt $ l, ArithOp $ Mod l, mempty)
  elaborate (Max      l) _ = return (Just $ tInt .-> tInt .-> tInt $ l, ArithOp $ Max l, mempty)
  elaborate (Min      l) _ = return (Just $ tInt .-> tInt .-> tInt $ l, ArithOp $ Min l, mempty)
  elaborate (Exp      l) _ = return (Just $ tInt .-> tInt .-> tInt $ l, ArithOp $ Exp l, mempty)
  elaborate (Hash     l) _ = return (Just $ tBool .-> tInt $ l, ArithOp $ Hash l, mempty)
  elaborate (PointsTo l) _ = return (Just $ tInt .-> tInt .-> tInt $ l, ArithOp $ PointsTo l, mempty)
  elaborate (SConj    l) _ = return (Just $ tBool .-> tBool .-> tBool $ l, ArithOp $ SConj l, mempty)
  elaborate (SImp     l) _ = return (Just $ tBool .-> tBool .-> tBool $ l, ArithOp $ SImp l, mempty)

--------------------------------------------------------------------------------
-- Unification

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

bind :: MonadError TypeError m => Name -> Type -> Loc -> m (Map.Map Name Type)
bind x t l | occurs x t = throwError $ RecursiveType x t l
           | otherwise  = return (Map.singleton x t)

class CollectIds a where
    collectIds :: a -> ElabaratorM ()

instance CollectIds a => CollectIds [a] where
  collectIds xs = mapM_ collectIds xs

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
