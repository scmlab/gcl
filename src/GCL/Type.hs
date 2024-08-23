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
{-# LANGUAGE DeriveGeneric #-}

module GCL.Type where

import           Control.Monad.Except
import           Control.Monad.State.Lazy
import           Control.Monad.Writer.Lazy
import           Data.Bifunctor                 ( Bifunctor (second, first) )
import           Data.Functor
import           Data.Maybe                     ( fromJust )
import           Data.List
import           Data.Loc                       ( Loc(..)
                                                , locOf, Located, (<-->)
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
import GHC.Generics
import qualified Data.Ord as Ord
import Syntax.Abstract.Util (wrapTFunc)

--------------------------------------------------------------------------------
-- The elaboration monad

type ElaboratorM = StateT (FreshState, [(Index, Kind)], [(Index, TypeInfo)], [(Name, Type, [Type])]) (Except TypeError)

instance Counterous ElaboratorM where
  countUp = do
    (count, typeDefnInfo, typeInfo, patInfo) <- get
    put (succ count, typeDefnInfo, typeInfo, patInfo)
    return count

data TypeError
    = NotInScope Name
    | UnifyFailed Type Type Loc
    | KindUnifyFailed Kind Kind Loc -- TODO: Deal with this replication in a better way.
    | RecursiveType Name Type Loc
    | AssignToConst Name
    | UndefinedType Name
    | DuplicatedIdentifiers [Name]
    | RedundantNames [Name]
    | RedundantExprs [Expr]
    | MissingArguments [Name]
    | PatternArityMismatch {- Expected -} Int {- Actual -} Int Loc
    deriving (Show, Eq, Generic)

instance Located TypeError where
  locOf (NotInScope n               ) = locOf n
  locOf (UnifyFailed _ _ l          ) = l
  locOf (KindUnifyFailed _ _ l      ) = l
  locOf (RecursiveType _ _ l        ) = l
  locOf (AssignToConst         n    ) = locOf n
  locOf (UndefinedType         n    ) = locOf n
  locOf (DuplicatedIdentifiers ns   ) = locOf ns
  locOf (RedundantNames        ns   ) = locOf ns
  locOf (RedundantExprs        exprs) = locOf exprs
  locOf (MissingArguments      ns   ) = locOf ns
  locOf (PatternArityMismatch _ _ l ) = l

instance Substitutable (Subs Type) TypeInfo where
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
-- TODO: We should probably rewrite the `ElaboratorM` monad to be an `RWS` / `RWST` monad.
-- The current states except the counter are actually environments. Also, we can use the writer effect to collect several errors at once.

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
    (_, _, env, _) <- get
    -- Do a `foldlM` to elaborate multiple definitions together.
    (_, names, tys, _sub) <-
      foldlM (\(context, names, tys, sub) funcDefn -> do
        case funcDefn of
          (FuncDefn name expr) -> do
            (ty, _, sub1) <- elaborate expr context -- Calling `head` is safe for the meantime.
            unifySub <- unifyType (subst sub1 $ typeInfoToType (fromJust $ lookup (Index name) context)) (fromJust ty) (locOf name) -- the first `fromJust` should also be safe.
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
      -- This function splits the definitions into type definitions, function signatures, and function definitions.
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

      -- This function generalizes a plain type into a type scheme.
      -- Currently, we represent monotypes and type schemes as the same type, that is, `Type`.
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

-- TODO: We have to do kind checking everywhere instead of relying on unification.

-- Type definitions are processed here.
-- We follow the approach described in the paper "Kind Inference for Datatypes": https://dl.acm.org/doi/10.1145/3371121
-- TODO: Unfortuantely, this implementation is not correct yet.
-- We, SCM and Andy, encountered some difficulties when we tried to read the above paper.
-- I want to try to give some hints for future implementors why the implementation is not correct.
-- First of all, the `take` function and the `drop` function are suspicious. It's possible that their behavior is not what we expect.
-- Next, the `resolve` function is awkward. Both SCM and Andy cannot understand the theoretical reason of substituting a context into itself.
-- Andy will be very happy if this is properly implemented.

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
  modify (\(freshState, origTypeDefnInfos, origTypeInfos, patInfos) -> (freshState, resolvedInfos <> origTypeDefnInfos, newTypeInfos' <> origTypeInfos, patInfos))
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
        inferCtor env' tyName' tyParamNames freshNames (TypeDefnCtor conName params) = do
          let sub = Map.fromList . zip tyParamNames $ freshNames
          let sub' = Map.fromList . zip tyParamNames $ TMetaVar <$> freshNames <*> pure NoLoc
          let retTy = formTy (TData tyName' (locOf tyName')) (TMetaVar <$> tyParamNames <*> pure NoLoc)
          let ty = subst sub' $ wrapTFunc params retTy
          -- Here, we collect the patterns.
          let pat = (conName, subst sub' retTy, params)
          modify (\(freshState, typeDefnInfos, origInfos, pats) -> (freshState, typeDefnInfos, origInfos, pat : pats))
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
inferKind env (TFunc _ _ loc) = return (KStar loc, env)
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
  Typed CaseClause = T.CaseClause
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

runElaboration
  :: Elab a => a -> [(Index, TypeInfo)] -> Either TypeError (Typed a)
runElaboration a env = do
  ((_, elaborated, _), _state) <- runExcept (runStateT (elaborate a env) (0, mempty, mempty, mempty))
  Right elaborated

newtype TypeDefnInfo = TypeDefnInfo [Name]

toKindEnv :: [(Index, Kind)] -> KindEnv
toKindEnv infos = (\(Index name', kind) -> KindAnno name' kind) <$> infos

toKinded :: [(Index, Kind)] -> Type -> ElaboratorM (Kind, T.KindedType)
toKinded env ty = do
  case ty of
    TBase base loc -> return (KStar loc, T.TBase base (KStar loc) loc)
    TArray int ty loc -> do
      (kind, kindedTy) <- toKinded env ty
      _ <- unifyKind (toKindEnv env) kind (KStar loc) loc
      return (KStar loc, T.TArray int kindedTy loc)
    TTuple n -> return (kindFromArity n, T.TTuple n (kindFromArity n))
    TFunc l r loc -> do
      (lKind, kindedL) <- toKinded env l
      (rKind, kindedR) <- toKinded env r
      _ <- unifyKind (toKindEnv env) lKind (KStar loc) loc
      _ <- unifyKind (toKindEnv env) rKind (KStar loc) loc
      return (KStar loc, T.TFunc kindedL kindedR loc)
    TOp arrow@(Arrow loc) -> return (KFunc (KStar loc) (KFunc (KStar loc) (KStar loc) loc) loc, T.TOp arrow (KFunc (KStar loc) (KFunc (KStar loc) (KStar loc) loc) loc))
    TData name loc -> do
      case lookup (Index name) env of
        Just k -> return (k, T.TData name k loc)
        _ -> error "Shouldn't happen."
    TApp ty1 ty2 loc -> do
      (ty1Kind, kindedTy1) <- toKinded env ty1
      (ty2Kind, kindedTy2) <- toKinded env ty2
      case ty1Kind of
        KStar loc' -> throwError $ KindUnifyFailed (KStar loc') (KFunc (KMetaVar (Name "k" NoLoc)) (KStar NoLoc) NoLoc) loc
        KFunc kind1 kind2 loc -> do
          _ <- unifyKind (toKindEnv env) ty2Kind kind1 loc
          return (kind2, T.TApp kindedTy1 kindedTy2 loc)
        _ -> error "Shouldn't happen."
    TVar name loc -> do
      case lookup (Index name) env of
        Just k -> return (k, T.TVar name k loc)
        _ -> error "Shouldn't happen."
    TMetaVar name loc -> do
      case lookup (Index name) env of
        Just k -> return (k, T.TMetaVar name k loc)
        _ -> error "Shouldn't happen."

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
    (kind, kinded) <- toKinded infos ty
    if kind == KStar NoLoc then return () else throwError $ KindUnifyFailed kind (KStar NoLoc) (locOf kind)
    return (Nothing, T.FuncDefnSig name kinded expr' loc, mempty)
  elaborate (FuncDefn name expr) env = do
    (_, typed, _) <- elaborate expr env
    return (Nothing, T.FuncDefn name typed, mempty)

instance Elab TypeDefnCtor where
  elaborate (TypeDefnCtor name ts) _ = do
    return (Nothing, T.TypeDefnCtor name ts, mempty)

instance Elab Declaration where
  elaborate (ConstDecl names ty prop loc) env = do
    (_, infos, _, _) <- get
    (kind, _) <- toKinded infos ty
    if kind == KStar NoLoc then return () else throwError $ KindUnifyFailed kind (KStar NoLoc) (locOf kind)
    case prop of
      Just p -> do
        (_, p', _) <- elaborate p env
        return (Nothing, T.ConstDecl names ty (Just p') loc, mempty)
      Nothing -> return (Nothing, T.ConstDecl names ty Nothing loc, mempty)
  elaborate (VarDecl names ty prop loc) env = do
    (_, infos, _, _) <- get
    (kind, _) <- toKinded infos ty
    if kind == KStar NoLoc then return () else throwError $ KindUnifyFailed kind (KStar NoLoc) (locOf kind)
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
    (_, _, infos, _) <- get
    return (Nothing, T.Spec text range infos, mempty)
  elaborate (Proof text1 text2 range) _ = return (Nothing, T.Proof text1 text2 range, mempty)
  elaborate (Alloc var exprs loc) env = do
    ty <- checkAssign env var
    _ <- unifyType ty (tInt NoLoc) $ locOf var
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
    _ <- unifyType ty (tInt NoLoc) $ locOf name
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
    return (Just returnTy, subst sub1 (T.Lam bound (subst sub1 tv) typedExpr1 loc), sub1)
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
        (innerTy, innerTypedExpr, innerSub) <- elaborate inner $ zip (Index <$> bound) (subst (uniSub2 `compose` resSub) . ConstTypeInfo <$> tvs) <> newEnv'
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
  -- TODO: Add the typing derivation for `Case`.
  -- This implementation follows another GitHub repo, https://github.com/anton-k/hindley-milner-type-check
  -- It's worth noting that we follow the code in the above repo to instatiate the types late.
  -- However, our implementatation and theirs are still subtly different.
  -- Therefore, it might be the case that we're wrong (actually we are) but theirs is right.
  elaborate (Case expr clauses loc) env = do
    (exprTy, typedExpr, exprSub) <- elaborate expr env
    (ty, typed, sub) <- inferClauses clauses (fromJust exprTy) exprSub
    return (Just $ subst sub ty, subst sub $ T.Case typedExpr typed loc, sub)
    where
      inferClauses :: [CaseClause] -> Type -> Subs Type -> ElaboratorM (Type, [Typed CaseClause], Subs Type)
      inferClauses clauses' ty sub = do
        (sub', _, tyRes', as) <- foldM go (sub, ty, Nothing, []) clauses'
        return (fromJust tyRes', reverse as, sub')
        where
          go :: (Subs Type, Type, Maybe Type, [Typed CaseClause]) -> CaseClause -> ElaboratorM (Subs Type, Type, Maybe Type, [Typed CaseClause])
          go (sub, tyTop, mTyPrevRhs, res) clause = do
            (tyClause, tyExpected, typedClause, subClause) <- inferClause clause tyTop
            let sub' = subClause `compose` sub
            sub'' <- unifyType (subst sub' tyTop) (subst sub' tyExpected) (locOf tyClause)
            case mTyPrevRhs of
              Nothing -> return (sub'' `compose` sub', subst (sub'' `compose` sub') tyTop, Just $ subst (sub'' `compose` sub') tyClause, subst (sub'' `compose` sub') typedClause : res)
              Just tyPrevRhs -> do
                sub''' <- unifyType (subst (sub'' `compose` sub') tyClause) (subst (sub'' `compose` sub') tyPrevRhs) (locOf tyClause)
                return (sub''' `compose` sub'' `compose` sub', subst (sub''' `compose` sub'' `compose` sub') tyTop, Just $ subst (sub''' `compose` sub'' `compose` sub') tyClause, subst (sub''' `compose` sub'' `compose` sub') typedClause : res)

      inferClause :: CaseClause -> Type -> ElaboratorM (Type, Type, Typed CaseClause, Subs Type)
      inferClause (CaseClause patts expr) ty = do
        (env', sub) <- patBind patts ty
        let env'' = env' <> subst sub env
        (exprTy, typedExpr, subExpr) <- elaborate expr env''
        let typedClause = T.CaseClause patts typedExpr
        return (fromJust exprTy, subst sub ty, typedClause, subExpr <> sub)
          where
            -- TODO: Check for redundent names.
            patBind :: Pattern -> Type -> ElaboratorM ([(Index, TypeInfo)], Subs Type)
            patBind pat ty = do
              (_, _, _, patInfos) <- get
              case pat of
                PattLit lit -> do
                  sub <- unifyType ty (TBase (baseTypeOfLit lit) (locOf lit)) (locOf ty)
                  return (mempty, sub)
                PattBinder na -> return ([(Index na, ConstTypeInfo ty)], mempty)
                PattWildcard _ -> return mempty
                PattConstructor patName subpats -> do
                  case find (\(name, _, _) -> name == patName) patInfos of
                    Nothing -> throwError $ NotInScope patName
                    Just (_, input, outputs) -> do
                      instantiated <- mapM instantiate (input : outputs) -- TODO: Important! We probably have to instantiate the types earlier.
                      let (input' : outputs') = instantiated
                      sub <- unifyType ty input' (locOf input')
                      if length subpats /= length outputs' then throwError $ PatternArityMismatch (length outputs) (length subpats) (locOf pat)
                      else do
                        list <- zipWithM patBind subpats (subst sub outputs')
                        let (envs, subs) = unzip list
                        return (mconcat envs, mconcat (sub : subs))

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

class Substitutable a b where
  subst :: a -> b -> b

compose :: Substitutable (Subs a) a => Subs a -> Subs a -> Subs a
s1 `compose` s2 = s1 <> Map.map (subst s1) s2

instance (Substitutable a b, Functor f) => Substitutable a (f b) where
  subst = fmap . subst

instance Substitutable (Subs Type) Type where
  subst _ t@TBase{}        = t
  subst s (TArray i t l  ) = TArray i (subst s t) l
  subst _ (TTuple arity  ) = TTuple arity
  subst s (TFunc l r loc ) = TFunc (subst s l) (subst s r) loc
  subst _ (TOp op        ) = TOp op
  subst s (TApp l r loc  ) = TApp (subst s l) (subst s r) loc
  subst _ t@TData{}        = t
  subst s t@(TVar n _    ) = Map.findWithDefault t n s
  subst s t@(TMetaVar n _) = Map.findWithDefault t n s

instance Substitutable (Subs Type) T.Expr where
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
  subst s (T.Case expr clauses loc) = T.Case (subst s expr) (subst s <$> clauses) loc
  subst s (T.Subst expr pairs) = T.Subst (subst s expr) (subst s <$> pairs)

instance Substitutable (Subs Type) T.CaseClause where
  subst s (T.CaseClause pat expr) = T.CaseClause pat (subst s expr)

instance Substitutable (Subs Type) T.Chain where
  subst s (T.Pure expr) = T.Pure $ subst s expr
  subst s (T.More ch op ty expr) = T.More (subst s ch) op (subst s ty) (subst s expr)