{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE LambdaCase #-}

module GCL.Type where

import           Control.Monad.Except
import           Control.Monad.State.Lazy
import           Control.Monad.Writer.Lazy
import           Data.Bifunctor                 ( Bifunctor (second, first) )
import           Data.Functor
import           Data.Maybe                     ( fromJust )
import           Data.List
import           Data.Loc                       ( Loc(..)
                                                , locOf
                                                )
import           Data.Foldable                  ( foldlM )
import qualified Data.Set                      as Set
import qualified Data.Map                      as Map
import           GCL.Common
import           Prelude                 hiding ( EQ
                                                , GT
                                                , LT
                                                )

import           Syntax.Abstract
import           Syntax.Common
import qualified Syntax.Typed                  as T

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

instance Substitutable Type TypeInfo where
  subst s (TypeDefnCtorInfo t) = TypeDefnCtorInfo (subst s t)
  subst s (ConstTypeInfo    t) = ConstTypeInfo (subst s t)
  subst s (VarTypeInfo      t) = VarTypeInfo (subst s t)

duplicationCheck :: (MonadError TypeError m) => [Name] -> m ()
duplicationCheck ns =
  let ds = map head . filter ((> 1) . length) . groupBy (\(Name text1 _)(Name text2 _) -> text1 == text2) $ ns
  in  if null ds
        then return ()
        else throwError . DuplicatedIdentifiers $ ds

--------------------------------------------------------------------------------
-- Collecting ids
-- The ids are collected by `CollectIds` into the state wrapped by `ElaboratorM`.
-- `collectIds` are thus called in the beginning of elaborating the whole program.

class CollectIds a where
    collectIds :: a -> ElaboratorM ()

{-
instance CollectIds a => CollectIds [a] where
  collectIds xs = mapM_ collectIds xs
-}


-- Currently (and hopefully in the future), we only modify the state of `ElaboratorM` during `collectIds`.

instance CollectIds [Definition] where -- TODO: Collect patterns.
  collectIds defns = do
    -- First, we split variants of definitions because different kinds of definitions need to be processed differently.
    let (typeDefns, funcSigs, funcDefns) = split defns
    -- Check if there are duplications of definitions.
    -- While checking signatures and function definitions, we take account of the names of (term) constructors.
    duplicationCheck $ (\(TypeDefn name _ _ _) -> name) <$> typeDefns
    duplicationCheck $ ((\(FuncDefnSig name _ _ _) -> name) <$> funcSigs) <> gatherCtorNames typeDefns
    duplicationCheck $ ((\(FuncDefn name _) -> name) <$> funcDefns) <> gatherCtorNames typeDefns
    -- Gather the type definitions.
    -- Type definitions are collected first because signatures and function definitions may depend on them.
    collectTypeDefns typeDefns
    -- Add signatures into the state one by one.
    collectFuncSigs funcSigs
    -- Get the original explicit signatures.
    -- We will try to restrict polymorphic functions to the types of the corresponding signatures.
    (_, _, infos, _) <- get
    let sigEnv = second typeInfoToType <$> infos
    -- Give each function definition a fresh name.
    let defined = concatMap (\case
                                (FuncDefn name _exprs) -> [Index name]
                                _ -> []
                            ) defns
    freshVars <- replicateM (length defined) freshVar
    let gathered = second ConstTypeInfo <$> zip defined freshVars
    modify (\(freshState, typeDefnInfos, origInfos, patInfos) -> (freshState, typeDefnInfos, gathered <> origInfos, patInfos))
    -- Get the previously gathered type infos.
    (_, _, env) <- get
    -- Do a `foldlM` to elaborate multiple definitions together.
    (_, names, tys, _sub) <-
      foldlM (\(context, names, tys, sub) funcDefn -> do
        case funcDefn of
          (FuncDefn name expr) -> do
            (ty, _, sub1) <- elaborate expr context -- Calling `head` is safe for the meantime.
            unifySub <- unifyType (subst sub1 (fromJust $ lookup (Index name) context)) (fromJust ty) (locOf name) -- the first `fromJust` should also be safe.
            -- We see if there are signatures restricting the type of function definitions.
            case lookup (Index name) sigEnv of
              -- If there is, we save the restricted type.
              Just ty' -> do
                _ <- unifyType (fromJust ty) ty' (locOf name)
                return (subst (unifySub `compose` sub1) context, name : names, subst unifySub <$> (ty' : (subst sub1 <$> tys)), unifySub `compose` sub1 `compose` sub)
              -- If not, we proceed as if it's normal.
              Nothing -> return (subst (unifySub `compose` sub1) context, name : names, subst unifySub <$> (subst sub1 (fromJust ty) : (subst sub1 <$> tys)), unifySub `compose` sub1 `compose` sub)
          _ -> return (context, names, tys, sub)
      ) (env, mempty, mempty, mempty) funcDefns
    -- Generalize the types of definitions, i.e. change free type variables into metavariables.
    mapM_ (\(name, ty) -> do
            ty' <- generalize ty (second typeInfoToType <$> env) -- TODO: Why???
            let info = (Index name, ConstTypeInfo ty')
            modify (\(freshState, typeDefnInfos, origInfos, patInfos) -> (freshState, typeDefnInfos, info : origInfos, patInfos))
          ) (zip names tys)
    where
      split :: [Definition] -> ([Definition], [Definition], [Definition])
      split defs = foldr (
          \def (typeDefns, sigs, funcDefns)  -> case def of
            d @ TypeDefn {} -> (d : typeDefns, sigs, funcDefns)
            d @ FuncDefnSig {} -> (typeDefns, d : sigs, funcDefns)
            d @ FuncDefn {} -> (typeDefns, sigs, d : funcDefns)
        ) mempty defs

      gatherCtorNames :: [Definition] -> [Name]
      gatherCtorNames defs = do
        def <- defs
        case def of
          TypeDefn _ _ ctors _ -> do
            ctor <- ctors
            let TypeDefnCtor name _ = ctor
            return name
          _ -> []

      collectFuncSigs :: [Definition] -> ElaboratorM ()
      collectFuncSigs funcSigs =
        mapM_ (\(FuncDefnSig n t _ _) -> do
          let infos = (Index n, ConstTypeInfo t)
          modify (\(freshState, typeDefnInfos, origInfos, patInfos) -> (freshState, typeDefnInfos, infos : origInfos, patInfos))
        ) funcSigs

      generalize :: Fresh m => Type -> TypeEnv -> m Type
      generalize ty' env' = do
        let free = Set.toList (freeVars ty') \\ Set.toList (freeVars env')
        metaVars <- replicateM (length free) freshMetaVar
        let sub = zip free metaVars
        return $ subst (Map.fromList sub) ty'

instance CollectIds Declaration where
  collectIds (ConstDecl ns t _ _) = modify (\(freshState, typeDefnInfos, origInfos, patInfos) -> (freshState, typeDefnInfos, infos <> origInfos, patInfos))
    where infos = map ((, ConstTypeInfo t) . Index) ns
  collectIds (VarDecl   ns t _ _) = modify (\(freshState, typeDefnInfos, origInfos, patInfos) -> (freshState, typeDefnInfos, infos <> origInfos, patInfos))
    where infos = map ((, VarTypeInfo t) . Index) ns


--------------------------------------------------------------------------------
-- Elaboration


-- The type family `Typed` turns data into its typed version.
type family Typed untyped where
  Typed Definition = T.Definition
  Typed Declaration = T.Declaration
  Typed TypeDefnCtor = T.TypeDefnCtor
  Typed Program = T.Program
  Typed Stmt = T.Stmt
  Typed GdCmd = T.GdCmd
  Typed Expr = T.Expr
  Typed Chain = T.Chain
  Typed Name = Name
  Typed ChainOp = Op
  Typed ArithOp = Op
  Typed TypeOp = Op
  Typed Type = ()
  Typed Interval = ()
  Typed Endpoint = ()
  Typed [a] = [Typed a]
  Typed (Maybe a) = Maybe (Typed a)

class Located a => Elab a where
    elaborate :: a -> [(Index, TypeInfo)] -> ElaboratorM (Maybe Type, Typed a, Subs Type)


-- Note that we pass the collected ids into each sections of the program.
-- After `collectIds`, we don't need to change the state.
instance Elab Program where
  elaborate (Program defns decls exprs stmts loc) env = do
    mapM_ collectIds decls
    -- The `reverse` here shouldn't be needed now. In the past, it was a trick to make things work.
    -- I still keep it as-is in case of future refactoring / rewriting.
    collectIds $ reverse defns
    (_, _, infos, _) <- get
    typedDefns <- mapM (\defn -> do
                          typedDefn <- elaborate defn $ env <> infos
                          let (_, typed, _) = typedDefn
                          return typed
                       ) defns
    typedDecls <- mapM (\decl -> do
                          typedDecl <- elaborate decl $ env <> infos
                          let (_, typed, _) = typedDecl
                          return typed
                       ) decls
    typedExprs <- mapM (\expr -> do
                          typedExpr <- elaborate expr $ env <> infos
                          let (_, typed, _) = typedExpr
                          return typed
                       ) exprs
    typedStmts <- mapM (\stmt -> do
                          typedStmt <- elaborate stmt $ env <> infos
                          let (_, typed, _) = typedStmt
                          return typed
                       ) stmts
    return (Nothing, T.Program typedDefns typedDecls typedExprs typedStmts loc, mempty)
   where
    collectTCon (TypeDefn n args _ _) = [(Index n, TypeDefnInfo args)]
    collectTCon _                     = []

instance Elab Definition where
  elaborate (TypeDefn name args ctors loc) env = do
    let m = Set.fromList args
    mapM_ (\(TypeDefnCtor _ ts) -> mapM_ (scopeCheck m) ts) ctors
    ctors' <- mapM (\ctor -> do
                      (_, typed, _) <- elaborate ctor env
                      return typed
                   ) ctors
    return (Nothing, T.TypeDefn name args ctors' loc, mempty)
    where
      scopeCheck :: MonadError TypeError m => Set.Set Name -> Type -> m ()
      scopeCheck ns t = mapM_ (\n -> if Set.member n ns then return () else throwError $ NotInScope n) (freeVars t)
  elaborate (FuncDefnSig name ty maybeExpr loc) env = do
    expr' <- mapM (\expr -> do
                    (_, typed, _) <- elaborate expr env
                    return typed
                  ) maybeExpr
    (_, infos, _, _) <- get
    kinded <- toKinded infos ty
    kind <- kindCheck kinded
    if kind == KStar NoLoc then return () else throwError $ KindUnifyFailed kind (KStar NoLoc) (locOf kind)
    return (Nothing, T.FuncDefnSig name kinded expr' loc, mempty)
    where
      toKinded :: [(Index, Kind)] -> Type -> ElaboratorM Typed.KindedType
      toKinded env ty = do
        case ty of
          TBase base loc -> return $ Typed.TBase base (KStar loc) loc
          TArray int ty loc -> do
            kindedTy <- toKinded env ty
            return $ Typed.TArray int kindedTy loc
          TTuple n -> return $ Typed.TTuple n (kindFromArity n)
          TOp arrow@(Arrow _) -> return $ Typed.TOp arrow (KFunc (KStar loc) (KFunc (KStar loc) (KStar loc) loc) loc)
          TData name loc -> do
            case lookup (Index name) env of
              Just k -> return $ Typed.TData name k loc
              _ -> error "Shouldn't happen."
          TApp ty1 ty2 loc -> do
            kindedTy1 <- toKinded env ty1
            kindedTy2 <- toKinded env ty2
            return $ Typed.TApp kindedTy1 kindedTy2 loc
          TVar name loc -> do
            case lookup (Index name) env of
              Just k -> return $ Typed.TVar name k loc
              _ -> error "Shouldn't happen."
          TMetaVar name loc -> do
            case lookup (Index name) env of
              Just k -> return $ Typed.TMetaVar name k loc
              _ -> error "Shouldn't happen."

      kindCheck :: Typed.KindedType -> ElaboratorM Kind
      kindCheck kinded = case kinded of
        Typed.TBase _ kind _ -> return kind
        Typed.TArray _ kinded _ -> kindCheck kinded
        Typed.TTuple _ kind -> return kind
        Typed.TOp _ kind -> return kind
        Typed.TData _ kind _ -> return kind
        Typed.TApp kinded1 kinded2 _ -> do
          kind1 <- kindCheck kinded1
          kind2 <- kindCheck kinded2
          applyKind kind1 kind2
        Typed.TVar _ kind _ -> return kind
        Typed.TMetaVar _ kind _ -> return kind

      applyKind :: Kind -> Kind -> ElaboratorM Kind
      applyKind (KFunc left right loc) k2 = do
        _ <- unifyKind mempty left k2 loc
        return right
      applyKind k _ = throwError $ NotKFunc k (locOf k)

  elaborate (FuncDefn name expr) env = do
    (_, typed, _) <- elaborate expr env
    return (Nothing, T.FuncDefn name typed, mempty)

instance Elab TypeDefnCtor where
  elaborate (TypeDefnCtor name ts) _ = do
    return (Nothing, T.TypeDefnCtor name ts, mempty)

instance Elab Declaration where
  elaborate (ConstDecl names ty prop loc) env =
    case prop of
      Just p -> do
        (_, p', _) <- elaborate p env
        return (Nothing, T.ConstDecl names ty (Just p') loc, mempty)
      Nothing -> return (Nothing, T.ConstDecl names ty Nothing loc, mempty)
  elaborate (VarDecl names ty prop loc) env =
    case prop of
      Just p -> do
        (_, p', _) <- elaborate p env
        return (Nothing, T.VarDecl names ty (Just p') loc, mempty)
      Nothing -> return (Nothing, T.VarDecl names ty Nothing loc, mempty)

-- This function is used during elaborating stmts.
checkAssign :: [(Index, TypeInfo)] -> Name -> ElaboratorM Type
checkAssign infos name = do
  case lookup (Index name) infos of
    Just (VarTypeInfo t) -> pure t
    Just _               -> throwError $ AssignToConst name
    Nothing              -> throwError $ NotInScope name

instance Elab Stmt where
  elaborate (Skip  loc     ) _ = return (Nothing, T.Skip loc, mempty)
  elaborate (Abort loc     ) _ = return (Nothing, T.Abort loc, mempty)
  elaborate (Assign names exprs loc) env = do
    duplicationCheck names
    let ass = zip names exprs
    let an  = length ass
    if
      | an < length exprs -> throwError $ RedundantExprs (drop an exprs)
      | an < length names -> throwError $ RedundantNames (drop an names)
      | otherwise      -> do
        exprs' <- mapM (\(name, expr) -> do
                          ty <- checkAssign env name
                          (ty', typedExpr, _) <- elaborate expr env
                          _ <- unifyType ty (fromJust ty') $ locOf expr
                          return typedExpr
                       ) ass
        return (Nothing, T.Assign names exprs' loc, mempty)
  elaborate (AAssign arr index e loc) env = do
    tv <- freshVar
    (arrTy, typedArr, arrSub) <- elaborate arr env
    (indexTy, typedIndex, indexSub) <- elaborate index $ subst arrSub env
    uniSubIndex <- unifyType (subst indexSub $ fromJust indexTy) (tInt NoLoc) (locOf index)
    -- TODO: Wrap type-level application of operators into a function.
    uniSubArr <- unifyType (subst (indexSub `compose` uniSubIndex) (fromJust arrTy)) (tInt NoLoc ~-> tv) (locOf arr)
    (eTy, typedE, eSub) <- elaborate e $ subst (uniSubArr `compose` uniSubIndex `compose` indexSub `compose` arrSub) env
    _ <- unifyType (subst eSub $ fromJust eTy) (subst uniSubArr tv) (locOf e)
    return (Nothing, T.AAssign typedArr typedIndex typedE loc, mempty)
  elaborate (Assert expr loc        ) env = do
    (ty, _, sub) <- elaborate expr env
    _ <- unifyType (subst sub $ fromJust ty) (tBool NoLoc) (locOf expr)
    (_, typedExpr, _) <- elaborate expr env
    return (Nothing, T.Assert typedExpr loc, mempty)
  elaborate (LoopInvariant e1 e2 loc) env = do
    (ty1, _, sub1) <- elaborate e1 env
    _ <- unifyType (subst sub1 $ fromJust ty1) (tBool NoLoc) (locOf e1)
    (_, e1', _) <- elaborate e1 env
    (ty2, _, sub2) <- elaborate e2 env
    _ <- unifyType (subst sub2 $ fromJust ty2) (tInt NoLoc) (locOf e2)
    (_, e2', _) <- elaborate e2 env
    return (Nothing, T.LoopInvariant e1' e2' loc, mempty)
  elaborate (Do gds loc) env = do
    gds' <- mapM (\gd -> do
                  (_, typed, _) <- elaborate gd env
                  return typed
                 ) gds
    return (Nothing, T.Do gds' loc, mempty)
  elaborate (If gds loc) env = do
    gds' <- mapM (\gd -> do
                  (_, typed, _) <- elaborate gd env
                  return typed
                 ) gds
    return (Nothing, T.If gds' loc, mempty)
  elaborate (Spec text range) _ = do
    (_, _, infos) <- get
    return (Nothing, T.Spec text range infos, mempty)
  elaborate (Proof text1 text2 range) _ = return (Nothing, T.Proof text1 text2 range, mempty)
  elaborate (Alloc var exprs loc) env = do
    ty <- checkAssign env var
    _ <- unifies ty (tInt NoLoc) $ locOf var
    typedExprs <-
      mapM
        (\expr -> do
          (ty', typedExpr, _) <- elaborate expr env
          _ <- unifyType (fromJust ty') (tInt NoLoc) (locOf expr)
          return typedExpr
        ) exprs
    return (Nothing, T.Alloc var typedExprs loc, mempty)
  elaborate (HLookup name expr loc) env = do
    ty <- checkAssign env name
    _ <- unifies ty (tInt NoLoc) $ locOf name
    (ty', typedExpr, _) <- elaborate expr env
    _ <- unifyType (fromJust ty') (tInt NoLoc) (locOf expr)
    return (Nothing, T.HLookup name typedExpr loc, mempty)
  elaborate (HMutate left right loc) env = do
    (ty, typedLeft, _) <- elaborate left env
    _ <- unifyType (fromJust ty) (tInt NoLoc) (locOf left)
    (ty', typedRight, _) <- elaborate right env
    _ <- unifyType (fromJust ty') (tInt NoLoc) (locOf right)
    return (Nothing, T.HMutate typedLeft typedRight loc, mempty)
  elaborate (Dispose expr loc) env = do
    (ty, typedExpr, _) <- elaborate expr env
    _ <- unifyType (fromJust ty) (tInt NoLoc) (locOf expr)
    return (Nothing, T.Dispose typedExpr loc, mempty)
  elaborate Block {} env = undefined -- TODO: Implement blocks.

instance Elab GdCmd where
  elaborate (GdCmd expr stmts loc) env = do
    (ty, _, sub) <- elaborate expr env
    _ <- unifyType (subst sub $ fromJust ty) (tBool NoLoc) (locOf expr)
    (_, e', _) <- elaborate expr env
    s' <- mapM (\stmt -> do
                  (_, typed, _) <- elaborate stmt env
                  return typed
               ) stmts
    return (Nothing, T.GdCmd e' s' loc, mempty)

instantiate :: Fresh m => Type -> m Type
instantiate ty = do
  let freeMeta = Set.toList (freeMetaVars ty)
  new <- replicateM (length freeMeta) freshVar
  return $ subst (Map.fromList $ zip freeMeta new) ty

-- You can freely use `fromJust` below to extract the underlying `Type` from the `Maybe Type` you got.
-- You should also ensure that `elaborate` in `Elab Expr` returns a `Just` when it comes to the `Maybe Type` value.
-- TODO: Maybe fix this?

-- The typing rules below are written by SCM.

-- Γ ⊢ e ↑ (s, u)
-- v = unify (u, t)
---- Checking ------
-- Γ ⊢ e : t ↓ (v . s)

instance Elab Expr where
  elaborate (Lit lit loc) _ = let ty = litTypes lit loc in return (Just ty, T.Lit lit ty loc, mempty)
  -- x : t ∈ Γ
  -- t ⊑ u
  ---- Var, Const, Op --
  -- Γ ⊢ x ↑ (∅, t)
  elaborate (Var x loc) env = case lookup (Index x) env of
      Just info -> do
        ty' <- instantiate $ typeInfoToType info
        return (Just ty', T.Var x ty' loc, mempty)
      Nothing -> throwError $ NotInScope x
  elaborate (Const x loc) env = case lookup (Index x) env of
      Just info -> do
        ty' <- instantiate $ typeInfoToType info
        return (Just ty', T.Const x ty' loc, mempty)
      Nothing -> throwError $ NotInScope x
  elaborate (Op o) env = do
    (ty, op, sub) <- elaborate o env
    ty' <- instantiate $ fromJust ty
    return (Just ty', subst sub (T.Op op ty'), sub)
  elaborate (Chain ch) env = (\(ty, typed, sub) -> (ty, T.Chain typed, sub)) <$> elaborate ch env
  -- Γ ⊢ e1 ↑ (s1, t1)
  -- s1 Γ ⊢ e2 ↑ (s2, t2)
  -- b fresh   v = unify (s2 t1, t2 -> b)
  ---- App -----------------------------------
  -- Γ ⊢ e1 e2 ↑ (v . s2 . s1, v b)
  elaborate (App e1 e2 loc) env = do
    tv <- freshVar
    (ty1, typedExpr1, sub1) <- elaborate e1 env
    (ty2, typedExpr2, sub2) <- elaborate e2 $ subst sub1 env
    sub3 <- unifyType (subst sub2 $ fromJust ty1) (fromJust ty2 ~-> tv) loc
    let sub = sub3 `compose` sub2 `compose` sub1
    return (Just $ subst sub3 tv, subst sub (T.App typedExpr1 typedExpr2 loc), sub)
  -- a fresh
  -- Γ, x : a ⊢ e ↑ (s, t)
  ---- Lam ----------------------
  -- Γ ⊢ (λ x . e) ↑ (s, s a -> t)
  elaborate (Lam bound expr loc) env = do
    tv <- freshVar
    let newEnv = (Index bound, ConstTypeInfo tv) : env
    (ty1, typedExpr1, sub1) <- elaborate expr newEnv
    let returnTy = subst sub1 tv ~-> fromJust ty1
    return (Just returnTy, subst sub1 (Typed.Lam bound (subst sub1 tv) typedExpr1 loc), sub1)
  elaborate (Func name clauses l) env = undefined
  -- Γ ⊢ e1 ↑ (s1, t1)
  -- s1 Γ ⊢ e2 ↑ (s2, t2)
  ---- Tuple -----------------------------
  -- Γ ⊢ (e1, e2) ↑ (s2 . s1, (s2 t1, t2))
  elaborate (Tuple xs) env = undefined
  elaborate (Quant quantifier bound restriction inner loc) env = do
    duplicationCheck bound
    tv <- freshVar
    (quantTy, quantTypedExpr, quantSub) <- elaborate quantifier env
    -- TODO: Write out the typing rule.
    case quantifier of
      Op (Hash _) -> do
        tvs <- replicateM (length bound) freshVar
        let newEnv = subst quantSub env
        (resTy, resTypedExpr, resSub) <- elaborate restriction $ zip (Index <$> bound) (ConstTypeInfo <$> tvs) <> newEnv
        uniSub2 <- unifyType (fromJust resTy) (tBool NoLoc) (locOf restriction)
        let newEnv' = subst (uniSub2 `compose` resSub) newEnv
        (innerTy, innerTypedExpr, innerSub) <- elaborate inner $ zip (Index <$> bound) (subst (uniSub2 `compose` resSub) <$> tvs) <> newEnv'
        uniSub3 <- unifyType (subst innerSub $ fromJust innerTy) (subst (uniSub2 `compose` resSub `compose` quantSub) tv) (locOf inner)
        let sub = uniSub3 `compose` innerSub `compose` uniSub2 `compose` resSub `compose` quantSub
        return (Just $ subst quantSub tv, subst sub (T.Quant quantTypedExpr bound resTypedExpr innerTypedExpr loc), sub)
      -- a fresh   Γ ⊢ ⊕ : (a -> a -> a) ↓ s⊕
      -- b fresh   s⊕ Γ, i : b ⊢ R : Bool ↓ sR
      -- sR (s⊕ Γ), i : sR b ⊢ B : sR (s⊕ a) ↓ sB
      ---- Quant -------------------------------------------
      -- Γ ⊢ ⟨⊕ i : R : B⟩ ↑ (sB . sR , sB (sR (s⊕ a)))
      _ -> do
        uniSub <- unifyType (subst quantSub $ fromJust quantTy) (tv ~-> tv ~-> tv) (locOf quantifier)
        tvs <- replicateM (length bound) freshVar
        let newEnv = subst (uniSub `compose` quantSub) env
        (resTy, resTypedExpr, resSub) <- elaborate restriction $ zip (Index <$> bound) (ConstTypeInfo <$> tvs) <> newEnv
        uniSub2 <- unifyType (fromJust resTy) (tBool NoLoc) (locOf restriction)
        let newEnv' = subst (uniSub2 `compose` resSub) newEnv
        (innerTy, innerTypedExpr, innerSub) <- elaborate inner $ zip (Index <$> bound) (subst (uniSub2 `compose` resSub) . ConstTypeInfo <$> tvs) <> newEnv'
        uniSub3 <- unifyType (subst innerSub $ fromJust innerTy) (subst (uniSub2 `compose` resSub `compose` uniSub `compose` quantSub) tv) (locOf inner)
        return (Just $ subst (uniSub3 `compose` innerSub `compose` uniSub2 `compose` resSub `compose` uniSub `compose` quantSub) tv,
                subst (uniSub3 `compose` innerSub `compose` uniSub2 `compose` resSub `compose` uniSub `compose` quantSub) (T.Quant quantTypedExpr bound resTypedExpr innerTypedExpr loc),
                uniSub3 `compose` innerSub `compose` uniSub2 `compose` resSub)
  elaborate (RedexShell _ expr) env = undefined
  elaborate (RedexKernel n _ _ _) env = undefined
  -- b fresh    Γ ⊢ a : Array .. of b ↓ sa
  -- sa Γ ⊢ i : Int ↓ si
  ---- ArrIdx ----------------------------
  -- Γ ⊢ a[i] ↑ (si . sa, si (sa b))
  elaborate (ArrIdx e1 e2 loc) env = do -- I didn't follow the above typing rules. Hopefully this is correct.
    tv <- freshVar
    (ty1, typedExpr1, sub1) <- elaborate e1 env
    (ty2, typedExpr2, sub2) <- elaborate e2 (subst sub1 env)
    sub3 <- unifyType (subst sub2 $ fromJust ty2) (tInt NoLoc) (locOf e2)
    sub4 <- unifyType (subst (sub3 `compose` sub2) (fromJust ty1)) (tInt NoLoc ~-> tv) loc
    let sub = sub4 `compose` sub3 `compose` sub2 `compose` sub1
    return (Just $ subst sub3 tv, subst sub (T.ArrIdx typedExpr1 typedExpr2 loc), sub)
  -- b fresh    Γ ⊢ a : Array .. of b ↓ sa
  -- sa Γ ⊢ i : Int ↓ si
  -- si (sa Γ) ⊢ e : si (sa b) ↓ se
  ---- ArrUpd --------------------------------------
  -- Γ ⊢ (a : i ↦ e) ↑ (se . si . sa, se (si (sa b)))
  elaborate (ArrUpd arr index e loc) env = do -- I didn't follow the above typing rules. Hopefully this is correct.
    tv <- freshVar
    (arrTy, typedArr, arrSub) <- elaborate arr env
    (indexTy, typedIndex, indexSub) <- elaborate index $ subst arrSub env
    uniSubIndex <- unifyType (subst indexSub $ fromJust indexTy) (tInt NoLoc) (locOf index)
    uniSubArr <- unifyType (subst (indexSub `compose` uniSubIndex) (fromJust arrTy)) (tInt NoLoc ~-> tv) (locOf arr)
    (eTy, typedE, eSub) <- elaborate e $ subst (uniSubArr `compose` uniSubIndex `compose` indexSub `compose` arrSub) env
    uniSubExpr <- unifyType (subst eSub $ fromJust eTy) (subst uniSubArr tv) (locOf e)
    let sub = uniSubExpr `compose` eSub `compose` uniSubArr `compose` uniSubIndex `compose` indexSub `compose` arrSub
    return (subst uniSubArr arrTy, subst sub (T.ArrUpd typedArr typedIndex typedE loc), sub)
  elaborate (Case expr clauses loc) env = do
    (exprTy, typedExpr, exprSub) <- elaborate expr env
    (ty, typed, sub) <- inferClauses clauses (fromJust exprTy) exprSub
    return (Just $ subst sub ty, subst sub $ Typed.Case typedExpr typed loc, sub)
    where
      inferClauses :: [CaseClause] -> Type -> Subs Type -> ElaboratorM (Type, [Typed CaseClause], Subs Type)
      inferClauses clauses' ty sub = do
        (sub', _, tyRes', as) <- foldM go (sub, ty, Nothing, []) clauses'
        return (fromJust tyRes', reverse as, sub')
        where
          go :: (Subs Type, Type, Maybe Type, [Typed CaseClause]) -> CaseClause -> ElaboratorM (Subs Type, Type, Maybe Type, [Typed CaseClause])
          go (sub, tyTop, mTyPrevRhs, res) clause = do
            (tyClause, tyExpected, typedClause, subClause) <- inferClause clause
            let sub' = subClause `compose` sub
            sub'' <- unifyType (subst sub' tyTop) (subst sub' tyExpected) (locOf tyClause)
            case mTyPrevRhs of
              Nothing -> return (sub'' `compose` sub', subst (sub'' `compose` sub') tyTop, Just $ subst (sub'' `compose` sub') tyClause, subst (sub'' `compose` sub') typedClause : res)
              Just tyPrevRhs -> do
                sub''' <- unifyType (subst (sub'' `compose` sub') tyClause) (subst (sub'' `compose` sub') tyPrevRhs) (locOf tyClause)
                return (sub''' `compose` sub'' `compose` sub', subst (sub''' `compose` sub'' `compose` sub') tyTop, Just $ subst (sub''' `compose` sub'' `compose` sub') tyClause, subst (sub''' `compose` sub'' `compose` sub') typedClause : res)
      
      inferClause :: CaseClause -> ElaboratorM (Type, Type, Typed CaseClause, Subs Type)
      inferClause (CaseClause (PattConstructor patName subnames) expr) = do
        (_, _, _, infos) <- get
        case filter (\(name, _, _) -> name == patName) infos of
          [] -> throwError $ NotInScope patName
          (_, input, outputs) : _ -> do
            instantiated <- mapM instantiate (input : outputs) -- TODO: We might have to instantiate the types earlier.
            let (input' : outputs') = instantiated
            if
              | length subnames < length outputs' -> throwError $ TooFewPatterns (drop (length subnames) outputs')
              | length subnames > length outputs' -> throwError $ TooManyPatterns (drop (length outputs') subnames)
              | otherwise      -> do
                let env' = zip (Index <$> subnames) outputs' <> env -- TODO: Check for redundent names.
                (exprTy, typedExpr, subExpr) <- elaborate expr env'
                let typedClause = Typed.CaseClause (PattConstructor patName subnames) typedExpr
                return (fromJust exprTy, input', typedClause, subExpr)
      inferClause _ = undefined -- FIXME: Add other patterns.

instance Elab Chain where -- TODO: Make sure the below implementation is correct.
  elaborate (More (More ch' op1 e1 loc1) op2 e2 loc2) env = do
    tv <- freshVar
    (_chainTy, typedChain, chainSub) <- elaborate (More ch' op1 e1 loc1) env
    (opTy, opTyped, opSub) <- elaborate op2 env
    opTy' <- instantiate $ fromJust opTy
    (ty1, _typedExpr1, sub1) <- elaborate e1 $ subst chainSub env
    (ty2, typedExpr2, sub2) <- elaborate e2 $ subst (chainSub <> sub1) env
    unifyTy <- unifyType (fromJust ty1 ~-> fromJust ty2 ~-> tv) opTy' loc2
    let sub = chainSub <> opSub <> sub1 <> sub2
    return (Just $ subst unifyTy tv, subst sub $ T.More typedChain opTyped (subst unifyTy opTy') typedExpr2, sub)
  elaborate (More (Pure e1 _loc1) op e2 loc2) env = do
    tv <- freshVar
    (opTy, opTyped, opSub) <- elaborate op env
    opTy' <- instantiate $ fromJust opTy
    (ty1, typedExpr1, sub1) <- elaborate e1 env
    (ty2, typedExpr2, sub2) <- elaborate e2 $ subst sub1 env
    unifyTy <- unifyType (fromJust ty1 ~-> fromJust ty2 ~-> tv) opTy' loc2
    let sub = unifyTy <> sub2 <> sub1 <> opSub
    return (Just $ subst unifyTy tv, subst sub $ T.More (T.Pure typedExpr1) opTyped (subst unifyTy opTy') typedExpr2, sub)
  elaborate (Pure _expr _loc) _ = error "Chain of length 1 shouldn't exist."

instance Elab ChainOp where
  elaborate (EQProp  l) _ = return (Just $ tBool .-> tBool .-> tBool $ l, ChainOp $ EQProp l, mempty)
  elaborate (EQPropU l) _ = return (Just $ tBool .-> tBool .-> tBool $ l, ChainOp $ EQPropU l, mempty)
  elaborate (EQ      l) _ = do
    x <- freshMetaVar
    return (Just $ const x .-> const x .-> tBool $ l, ChainOp $ EQ l, mempty)
  elaborate (NEQ  l) _ = do
    x <- freshMetaVar
    return (Just $ const x .-> const x .-> tBool $ l, ChainOp $ NEQ l, mempty)
  elaborate (NEQU l) _ = do
    x <- freshMetaVar
    return (Just $ const x .-> const x .-> tBool $ l, ChainOp $ NEQU l, mempty)
  elaborate (LTE  l) _ = return (Just $ tInt .-> tInt .-> tBool $ l, ChainOp $ LTE l, mempty)
  elaborate (LTEU l) _ = return (Just $ tInt .-> tInt .-> tBool $ l, ChainOp $ LTEU l, mempty)
  elaborate (GTE  l) _ = return (Just $ tInt .-> tInt .-> tBool $ l, ChainOp $ GTE l, mempty)
  elaborate (GTEU l) _ = return (Just $ tInt .-> tInt .-> tBool $ l, ChainOp $ GTEU l, mempty)
  elaborate (LT   l) _ = return (Just $ tInt .-> tInt .-> tBool $ l, ChainOp $ LT l, mempty)
  elaborate (GT   l) _ = return (Just $ tInt .-> tInt .-> tBool $ l, ChainOp $ GT l, mempty)

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

instance Elab TypeOp where
  elaborate (Arrow _) _ = undefined -- We do not have a kind system yet.

--------------------------------------------------------------------------------
-- Unification

unifyType :: MonadError TypeError m => Type -> Type -> Loc -> m (Subs Type)
unifyType (TBase t1 _) (TBase t2 _) _ | t1 == t2 = return mempty
unifyType (TArray _ t1 _) (TArray _ t2 _) l = unifyType t1 t2 l {-  | i1 == i2 = unifies t1 t2 -}
  -- SCM: for now, we do not check the intervals
-- view array of type `t` as function type of `Int -> t`
unifyType (TArray _ t1 _) (TApp (TApp (TOp (Arrow _)) i _) t2 _) l = do
  s1 <- unifyType i (tInt NoLoc) l
  s2 <- unifyType t1 t2 l
  return (s2 `compose` s1)
unifyType (TOp (Arrow _)) (TOp (Arrow _)) _ = pure mempty
unifyType t1@(TData name1 _) t2@(TData name2 _) l = if name1 == name2 then pure mempty else throwError $ UnifyFailed t1 t2 l
unifyType (TApp (TApp (TOp (Arrow _)) i _) t1 _) (TArray _ t2 _) l = do
  s1 <- unifyType i (tInt NoLoc) l
  s2 <- unifyType t1 t2 l
  return (s2 `compose` s1)
unifyType (TApp t1 t2 _) (TApp t3 t4 _) l = do
  s1 <- unifyType t1 t3 l
  s2 <- unifyType (subst s1 t2) (subst s1 t4) l
  return (s2 `compose` s1)
unifyType (TVar x _)     t              l = bind x t l
unifyType t              (TVar x _)     l = bind x t l
unifyType (TMetaVar x _) t              l = bind x t l
unifyType t              (TMetaVar x _) l = bind x t l
unifyType t1             t2             l = throwError $ UnifyFailed t1 t2 l

bind :: MonadError TypeError m => Name -> Type -> Loc -> m (Map.Map Name Type)
bind x t l | same t $ TVar x NoLoc = return mempty
           | occurs x t  = throwError $ RecursiveType x t l
           | otherwise   = return (Map.singleton x t)
  where
    same (TVar v1 _) (TVar v2 _) = v1 == v2
    same _ _ = False

--------------------------------------------------------------------------------
-- helper combinators

typeInfoToType :: TypeInfo -> Type
typeInfoToType (TypeDefnCtorInfo t) = t
typeInfoToType (ConstTypeInfo    t) = t
typeInfoToType (VarTypeInfo      t) = t

freshVar :: Fresh m => m Type
freshVar = TVar <$> freshName "Type.var" NoLoc <*> pure NoLoc

freshMetaVar :: Fresh m => m Type
freshMetaVar = TMetaVar <$> freshName "Type.metaVar" NoLoc <*> pure NoLoc

litTypes :: Lit -> Loc -> Type
litTypes (Num _) l = tInt l
litTypes (Bol _) l = tBool l
litTypes (Chr _) l = tChar l

tBool, tInt, tChar :: Loc -> Type
tBool = TBase TBool
tInt = TBase TInt
tChar = TBase TChar

(.->) :: (Loc -> Type) -> (Loc -> Type) -> (Loc -> Type)
(t1 .-> t2) l = TApp (TApp (TOp (Arrow NoLoc)) (t1 l) NoLoc) (t2 l) l
infixr 1 .->

(~->) :: Type -> Type -> Type
t1 ~-> t2 = TApp (TApp (TOp (Arrow NoLoc)) t1 NoLoc) t2 NoLoc
infixr 1 ~->

emptyInterval :: Interval
emptyInterval = Interval (Including zero) (Excluding zero) NoLoc
  where zero = Lit (Num 0) NoLoc

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
  subst s (TApp l r loc ) = TApp (subst s l) (subst s r) loc
  subst _ t@TData {}      = t
  subst s t@(TVar n _)    = Map.findWithDefault t n s
  subst s t@(TMetaVar n)  = Map.findWithDefault t n s

instance Substitutable Type T.Expr where
  subst s (T.Lit lit ty loc) = T.Lit lit (subst s ty) loc
  subst s (T.Var name ty loc) = T.Var name (subst s ty) loc
  subst s (T.Const name ty loc) = T.Const name (subst s ty) loc
  subst s (T.Op op ty) = T.Op op $ subst s ty
  subst s (T.Chain ch) = T.Chain $ subst s ch
  subst s (T.App e1 e2 loc) = T.App (subst s e1) (subst s e2) loc
  subst s (T.Lam name ty expr loc) = T.Lam name (subst s ty) (subst s expr) loc
  subst s (T.Quant quantifier vars restriction inner loc) = T.Quant (subst s quantifier) vars (subst s restriction) (subst s inner) loc
  subst s (T.ArrIdx arr index loc) = T.ArrIdx (subst s arr) (subst s index) loc
  subst s (T.ArrUpd arr index expr loc) = T.ArrUpd (subst s arr) (subst s index) (subst s expr) loc
  subst s (T.Subst e es) = T.Subst (subst s e) [(x, subst s ex) | (x, ex) <- es]

instance Substitutable Type T.Chain where
  subst s (T.Pure expr) = T.Pure $ subst s expr
  subst s (T.More ch op ty expr) = T.More (subst s ch) op (subst s ty) (subst s expr)
