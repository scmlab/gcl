{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module GCL.Kind where

import           Control.Monad.Except
import           Control.Monad.State.Lazy
import           Data.Bifunctor                 ( Bifunctor (second) )
import qualified Data.Ord                      as Ord
import           Data.List
import           Data.Loc                       ( (<-->)
                                                , Loc(..)
                                                , locOf
                                                )
import qualified Data.Map                      as Map
import           GCL.Common

import           Syntax.Abstract
import           Syntax.Abstract.Util
import           Syntax.Common

-- TODO: We have to do kind checking everywhere instead of relying on unification.

-- Type definitions are processed here.
-- We follow the approach described in the paper "Kind Inference for Datatypes": https://dl.acm.org/doi/10.1145/3371121
-- `collectTypeDefns` is an entry for "Typing Program", presented in page 53:8 in the paper. 
collectTypeDefns :: [Definition] -> ElaboratorM ()
collectTypeDefns typeDefns = do
  freshMetaVars <- replicateM (length typeDefns) freshKindName
  let annos = (\(TypeDefn name _args _ctors _loc, kinds) -> KindAnno name kinds) <$> zip typeDefns (KMetaVar <$> freshMetaVars)
  let initEnv = reverse annos <> reverse (UnsolvedUni <$> freshMetaVars)
  (newTypeInfos, newTypeDefnInfos) <- inferDataTypes (mempty, initEnv) ((\(TypeDefn name args ctors loc) -> (name, args, ctors, loc)) <$> typeDefns)
  let newTypeInfos' = second TypeDefnCtorInfo <$> newTypeInfos
  let newTypeDefnInfos' =
        (\case
          KindAnno name kind -> (Index name, kind)
          UnsolvedUni _name -> error "Unsolved KMetaVar."
          SolvedUni name kind -> (Index name, kind)) <$> defaultMeta newTypeDefnInfos
  let resolvedInfos = resolve newTypeDefnInfos' newTypeDefnInfos'
  modify (\(freshState, origTypeDefnInfos, origTypeInfos) -> (freshState, resolvedInfos <> origTypeDefnInfos, newTypeInfos' <> origTypeInfos))
  where
    -- This is an entry for "Typing Datatype Decl." several times, presented in page 53:8.
    inferDataTypes :: (TypeEnv, KindEnv) -> [(Name, [Name], [TypeDefnCtor], Loc)] -> ElaboratorM (TypeEnv, KindEnv)
    inferDataTypes (typeEnv, kindEnv) [] = do
      return (typeEnv, kindEnv)
    inferDataTypes (typeEnv, kindEnv) ((tyName, tyParams, ctors, loc) : dataTypesInfo) = do
      (typeEnv', kindEnv') <- inferDataType kindEnv tyName tyParams ctors loc
      (typeEnv'', kindEnv'') <- inferDataTypes (typeEnv', kindEnv') dataTypesInfo
      return (typeEnv <> typeEnv' <> typeEnv'', kindEnv'')
      
    -- This is the "defaulting" mechanism presented in the paper.
    defaultMeta :: KindEnv -> KindEnv
    defaultMeta env =
      (\case
        KindAnno name kind -> KindAnno name kind
        UnsolvedUni name -> SolvedUni name $ KStar $ locOf name
        SolvedUni name kind -> SolvedUni name kind
      ) <$> env

    -- This is an entry for "Typing Datatype Decl.", presented in page 53:8.
    inferDataType :: KindEnv -> Name -> [Name] -> [TypeDefnCtor] -> Loc -> ElaboratorM (TypeEnv, KindEnv)
    inferDataType env tyName tyParams ctors loc = do
      case find (isKindAnno tyName) env of
        Just (KindAnno _name k) -> do
          metaVarNames <- replicateM (length tyParams) freshKindName
          let newEnv = (UnsolvedUni <$> reverse metaVarNames) <> env
          -- We do a unification here.
          newEnv' <- unifyKind newEnv (subst env k) (wrapKFunc (KMetaVar <$> reverse metaVarNames) (KStar NoLoc)) loc
          let solvedEnv = (\(SolvedUni _ kind) -> kind) <$> take (length tyParams) newEnv'
          let envForInferingCtors = zipWith KindAnno (reverse tyParams) solvedEnv <> drop (length tyParams) newEnv'
          (inferedTypes, finalEnv) <- inferCtors envForInferingCtors tyName tyParams ctors
          let ctorIndices = (\(TypeDefnCtor name _) -> Index name) <$> ctors
          return (zip ctorIndices inferedTypes, drop (length tyParams) finalEnv)
        _ -> throwError $ UndefinedType tyName
      where
        wrapKFunc :: [Kind] -> Kind -> Kind
        wrapKFunc [] k = k
        wrapKFunc (k : ks) k' = wrapKFunc ks (KFunc k k' $ k <--> k')

        formTy con params =
          case params of
            [] -> con
            n : ns -> formTy (TApp con n $ con <--> n) ns

        -- This is an entry for "Typing Data Constructor Decl." several times, presented in page 53:8.
        inferCtors :: KindEnv -> Name -> [Name] -> [TypeDefnCtor] -> ElaboratorM ([Type], KindEnv)
        inferCtors env' _ _ [] = return (mempty, env')
        inferCtors env' tyName' tyParamNames ctors' = do
          -- Give type variables fresh names to prevent name collision.
          freshNames <- mapM freshName' $ (\(Name text _) -> "Type." <> text) <$> tyParamNames
          inferCtors' env' tyName' tyParamNames freshNames ctors'
          where
            -- Recursion happens here.
            inferCtors' :: KindEnv -> Name -> [Name] -> [Name] -> [TypeDefnCtor] -> ElaboratorM ([Type], KindEnv)
            inferCtors' env'' _ _ _ [] = return (mempty, env'')
            inferCtors' env'' tyName'' tyParamNames' freshNames (ctor : restOfCtors) = do
              (ty, newEnv) <- inferCtor env'' tyName'' tyParamNames' freshNames ctor
              (tys, anotherEnv) <- inferCtors' newEnv tyName'' tyParamNames' freshNames restOfCtors
              return (ty : tys, newEnv <> anotherEnv)

        -- This is an entry for "Typing Data Constructor Decl.", presented in page 53:8.
        inferCtor :: KindEnv -> Name -> [Name] -> [Name] -> TypeDefnCtor -> ElaboratorM (Type, KindEnv)
        inferCtor env' tyName' tyParamNames freshNames (TypeDefnCtor _conName params) = do
          let sub = Map.fromList . zip tyParamNames $ freshNames
          let sub' = Map.fromList . zip tyParamNames $ TMetaVar <$> freshNames <*> pure NoLoc
          let ty = subst sub' $ wrapTFunc params (formTy (TData tyName' (locOf tyName')) (TMetaVar <$> tyParamNames <*> pure NoLoc))
          -- Here, we enter the world for infering kinds.
          (_kind, env'') <- inferKind (renameKindEnv sub env') ty
          return (ty, env'')
          where
            renameKindEnv :: Map.Map Name Name -> KindEnv -> KindEnv
            renameKindEnv sub env'' =
              (\case
                KindAnno name kind -> case Map.lookup name sub of
                  Nothing -> KindAnno name kind
                  Just name' -> KindAnno name' kind
                UnsolvedUni name -> UnsolvedUni name
                SolvedUni name kind -> SolvedUni name kind
              ) <$> env''

    -- This function substitutes a context into itself.
    -- TODO: I (Andy) think the algorithm here is suboptimal.
    resolve :: [(Index, Kind)] -> [(Index, Kind)] -> [(Index, Kind)]
    resolve [] env = env
    resolve (pair : pairs) env = 
      let env' = substEnv pair env
      in resolve pairs env'
      where
        substEnv :: (Index, Kind) -> [(Index, Kind)] -> [(Index, Kind)]
        substEnv sub context = second (substSingle sub) <$> context
        substSingle :: (Index, Kind) -> Kind -> Kind
        substSingle sub@(index, kind') kind =
          case kind of
            KStar loc -> KStar loc
            KFunc kind1 kind2 loc -> KFunc (substSingle sub kind1) (substSingle sub kind2) loc
            KMetaVar name -> if Index name == index then kind' else kind

--------------------------------------------------------------------------------
-- Kind inference

kindFromArity :: Int -> Kind
kindFromArity 0 = KStar NoLoc
kindFromArity n = KFunc (KStar NoLoc) (kindFromArity $ n - 1) NoLoc

-- This is "Kinding" mentioned in 53:10 in the paper "Kind Inference for Datatypes".
inferKind :: KindEnv -> Type -> ElaboratorM (Kind, KindEnv)
inferKind env (TBase _ loc) = return (KStar loc, env)
inferKind env (TArray _ _ loc) = return (KStar loc, env)
inferKind env (TTuple int) = return (kindFromArity int, env)
inferKind env (TOp (Arrow loc)) = return (KFunc (KStar loc) (KFunc (KStar loc) (KStar loc) loc) loc, env)
inferKind env (TData name _) =
  case find (isKindAnno name) env of
    Just (KindAnno _name kind) -> return (kind, env)
    _ -> throwError $ UndefinedType name
inferKind env (TApp t1 t2 _) = do
  (k1, env') <- inferKind env t1
  (k2, env'') <- inferKind env' t2
  (k3, env''') <- inferKApp env'' (subst env'' k1) (subst env'' k2)
  return (k3 ,env''')
inferKind env (TVar name _) =
  case find (isKindAnno name) env of
    Just (KindAnno _name kind) -> return (kind, env)
    _ -> throwError $ UndefinedType name
inferKind env (TMetaVar name _) =
  case find (isKindAnno name) env of
    Just (KindAnno _name kind) -> return (kind, env)
    _ -> throwError $ UndefinedType name

-- This is "Application Kinding" mentioned in 53:10 in the paper "Kind Inference for Datatypes".
inferKApp :: KindEnv -> Kind -> Kind -> ElaboratorM (Kind, KindEnv)
inferKApp env (KFunc k1 k2 _) k = do
  env' <- unifyKind env k1 k $ k1 <--> k
  return (k2, env')
inferKApp env (KMetaVar a) k = do
  case nameIndex of
    Nothing -> throwError $ NotInScope a
    Just nameIndex' -> do
      a1 <- freshKindName
      a2 <- freshKindName
      let env' = let (list1, list2) = splitAt nameIndex' env
                 in list1 ++ [SolvedUni a $ KFunc (KMetaVar a1) (KMetaVar a2) $ a1 <--> a2, UnsolvedUni a2, UnsolvedUni a1] ++ tail list2
      env'' <- unifyKind env' (KMetaVar a1) k $ locOf k
      return (KMetaVar a2, env'')
  where
    nameIndex = findIndex (predicate a) env

    predicate name (UnsolvedUni name') = name == name'
    predicate _ _ = False
inferKApp _ k1 k2 = do
  ret <- freshKindName
  throwError $ KindUnifyFailed k1 (KFunc k2 (KMetaVar ret) $ locOf k2) $ locOf k1


-- This is "Kind Unification" mentioned in 53:10 in the paper "Kind Inference for Datatypes".
unifyKind :: KindEnv -> Kind -> Kind -> Loc -> ElaboratorM KindEnv
unifyKind env k1 k2 _ | k1 == k2 = return env
unifyKind env (KFunc k1 k2 _) (KFunc k3 k4 _) _ = do
  env' <- unifyKind env k1 k3 (locOf k1)
  unifyKind env' (subst env' k2) (subst env' k4) (locOf k2)
unifyKind env (KMetaVar a) k _ = do
  case aIndex of
    -- The below 4 lines have implementation different from what is written on the paper.
    -- I put the original (probably incorrect) implementation that I think is what the paper describes in comments.
    Nothing -> return env -- throwError $ NotInScope a
    Just _aIndex' -> do
      -- let (list1, list2) = splitAt aIndex' env
      (k2, env') <- promote env a k -- (k2, env') <- promote (list1 ++ tail list2) a k
      let aIndex2 = findIndex (predicate a) env'
      case aIndex2 of
        Nothing -> throwError $ NotInScope a
        Just aIndex2' -> do
          let (list1', list2') = splitAt aIndex2' env'
          return $ list1' ++ [SolvedUni a k2] ++ tail list2'
  where
    -- Notice the `fromJust`. This part crashes when `a` is not present in `env`. (They should)
    aIndex = findIndex (predicate a) env

    predicate name (UnsolvedUni name') = name == name'
    predicate _ _ = False
unifyKind env k (KMetaVar a) loc = unifyKind env (KMetaVar a) k loc
unifyKind _env k1 k2 loc = throwError $ KindUnifyFailed k1 k2 loc

-- This is "Promotion" mentioned in 53:10 in the paper "Kind Inference for Datatypes".
promote :: KindEnv -> Name -> Kind -> ElaboratorM (Kind, KindEnv)
promote env _ (KStar loc) = return (KStar loc, env)
promote env name (KFunc k1 k2 loc) = do
  (k3, env') <- promote env name k1
  (k4, env'') <- promote env' name (subst env' k2)
  return (KFunc k3 k4 loc, env'')
promote env a (KMetaVar b) =
  case compare aIndex bIndex of
    Ord.LT -> return (KMetaVar b, env)
    Ord.EQ -> error "Should not happen."
    Ord.GT -> do
      b1 <- freshKindName
      case aIndex of
        Nothing -> throwError $ NotInScope a
        Just aIndex' -> do
          let env' = let (list1, list2) = splitAt aIndex' env in list1 ++ [UnsolvedUni a, UnsolvedUni b1] ++ tail list2
          case bIndex of
            Nothing -> throwError $ NotInScope b
            Just bIndex' -> do
              let env'' = let (list1, list2) = splitAt bIndex' env' in list1 ++ [SolvedUni b (KMetaVar b1)] ++ tail list2
              return (KMetaVar b1, env'')
  where
    aIndex = findIndex (predicate a) env
    bIndex = findIndex (predicate b) env

    predicate name (UnsolvedUni name') = name == name'
    predicate _ _ = False

isKindAnno :: Name -> KindItem -> Bool
isKindAnno name (KindAnno name' _kind) = name == name'
isKindAnno _ _ = False

freshKindName :: ElaboratorM Name
freshKindName = freshName "Kind.metaVar" NoLoc

instance Substitutable KindEnv Kind where
  subst _ (KStar loc) = KStar loc
  subst env (KMetaVar name) = case find (predicate name) env of
    Just (SolvedUni _ kind) -> kind
    _ -> KMetaVar name
    where
      predicate name1 (SolvedUni name2 _kind) = name1 == name2
      predicate _ _ = False
  subst env (KFunc k1 k2 loc) = KFunc (subst env k1) (subst env k2) loc