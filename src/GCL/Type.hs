{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TupleSections #-}

module GCL.Type where

import           Control.Monad.Except
import           Control.Monad.RWS       hiding ( Sum )
import           Control.Monad.State            ( StateT(..)
                                                , evalStateT
                                                )
import           Data.Aeson                     ( ToJSON )
import           Data.List.NonEmpty             ( NonEmpty )
import qualified Data.List.NonEmpty            as NE
import           Data.Loc
import           Data.Map                       ( Map )
import qualified Data.Map                      as Map
import qualified Data.Maybe                    as Maybe
import qualified Data.Set                      as Set
import           Data.Functor
import           Data.List
import           GCL.Common
import           GHC.Generics                   ( Generic )
import           Prelude                 hiding ( Ordering(..) )
import           Syntax.Abstract
import           Syntax.Abstract.Util
import           Syntax.Common

data TypeError
  = NotInScope Name Loc
  | UnifyFailed Type Type Loc
  | RecursiveType Name Type Loc
  | NotFunction Type Loc
  | NotArray    Type Loc
  | NotEnoughExprsInAssigment (NonEmpty Name) Loc
  | TooManyExprsInAssigment (NonEmpty Expr) Loc
  | AssignToConst Name Loc
  | AssignToLet Name Loc
  | UndefinedType Name Loc
  | DuplicatedIdentifier Name Loc
  deriving (Show, Eq, Generic)

instance ToJSON TypeError

instance Located TypeError where
  locOf (NotInScope _ l               ) = l
  locOf (UnifyFailed   _ _ l          ) = l
  locOf (RecursiveType _ _ l          ) = l
  locOf (NotFunction               _ l) = l
  locOf (NotArray                  _ l) = l
  locOf (NotEnoughExprsInAssigment _ l) = l
  locOf (TooManyExprsInAssigment   _ l) = l
  locOf (AssignToConst             _ l) = l
  locOf (AssignToLet               _ l) = l
  locOf (UndefinedType             _ l) = l
  locOf (DuplicatedIdentifier      _ l) = l

------------------------------------------
-- type enviornment
------------------------------------------

-- TODO: loc handle or not ?
extend :: Map Name a -> (Name, a) -> TM (Map Name a)
extend env (x, s) = case Map.lookup x env of
  Nothing -> return $ Map.insert x s env
  Just _  -> throwError $ DuplicatedIdentifier x (locOf x)

extendType :: Map Name Type -> (Name, Type, Loc) -> TM (Map Name Type)
extendType env (n, t, l) = case Map.lookup n env of
  Nothing -> return $ Map.insert n t env
  Just t' -> do
    s <- unifies t t' l
    return $ Map.map (subst s) env

extendEnv :: Environment -> Environment -> TM Environment
extendEnv (Environment lds1 tds1 lctx1) (Environment lds2 tds2 lctx2) =
  do
      Environment
    <$>         lds1
    `extendMap` lds2
    <*>         tds1
    `extendMap` tds2
    <*>         lctx1
    `extendMap` lctx2
 where
  extendMap m1 m2 =
    Map.foldlWithKey (\m k v -> m >>= (`extend` (k, v))) (pure m1) m2

------------------------------------------
-- Substitution
------------------------------------------

------------------------------------------
-- type inference
------------------------------------------

data Environment = Environment
  { envLocalDefns   :: Map Name Type
  , envTypeDefns    :: Map Name ([Name], [TypeDefnCtor])
  , envLocalContext :: Map Name [Expr]
  }
  deriving (Eq, Show)

instance Semigroup Environment where
  (Environment lds1 tds1 lctx1) <> (Environment lds2 tds2 lctx2) =
    Environment (lds1 <> lds2) (tds1 <> tds2) (lctx1 <> lctx2)

instance Monoid Environment where
  mempty = Environment mempty mempty mempty

type Infer = RWST Environment [Constraint] FreshState (Except TypeError)
type TM = StateT FreshState (Except TypeError)

instance Fresh Infer where
  getCounter = get
  setCounter = put

unify :: Type -> Type -> Loc -> Infer ()
unify x y l = tell [(x, y, l)]

inferInEnv :: [(Name, Type)] -> Infer Type -> Infer Type
inferInEnv l m = do
  let scope = \env ->
        env { envLocalDefns = Map.fromList l `Map.union` envLocalDefns env }
  local scope m

infer :: Expr -> Infer Type
infer (Lit   lit l) = return (litTypes lit l)
infer (Var   x   _) = lookupInferEnv x
infer (Const c   _) = lookupInferEnv c
infer (Op o       ) = inferOpTypes o
infer (App (App (Op op@(ChainOp _)) e1 _) e2 l) = do
  top <- inferOpTypes op

  t1' <- case e1 of
    App (App (Op (ChainOp _)) _ _) e12 _ -> do
      _ <- infer e1
      infer e12
    _ -> infer e1

  t2' <- infer e2

  v   <- freshVar
  unify top (TFunc t1' (TFunc t2' v NoLoc) NoLoc) l

  return (TBase TBool l)
infer (App e1 e2 l) = do
  t1 <- infer e1
  t2 <- infer e2
  v  <- freshVar
  unify t1 (TFunc t2 v NoLoc) l
  return v
infer (Lam x e l) = do
  v <- freshVar
  t <- inferInEnv [(x, v)] (infer e)
  return (TFunc v t l)
infer (Quant qop iters rng t l) = do
  titers <- mapM (const freshVar) iters
  tr     <- inferInEnv [ (n, tn) | n <- iters, tn <- titers ] (infer rng)
  unify tr (TBase TBool NoLoc) (locOf rng)
  tt <- inferInEnv [ (n, tn) | n <- iters, tn <- titers ] (infer t)
  case qop of
    Op (ArithOp (Hash _)) -> do
      unify tt (TBase TBool NoLoc) (locOf t)
      return (TBase TInt l)
    op -> do
      to <- infer op
      x  <- freshVar
      unify to (TFunc x (TFunc x x NoLoc) NoLoc) (locOf qop)
      unify tt x (locOf t)
      return x
infer (Redex (Rdx _ expr)  ) = infer expr
infer (RedexStem name _ _ _) = lookupInferEnv name
infer (ArrIdx e1 e2 _      ) = do
  t1 <- infer e1
  let interval = case t1 of
        TArray itv _ _ -> itv
        _              -> emptyInterval
  t2 <- infer e2
  unify t2 (TBase TInt NoLoc) (locOf e2)
  v <- freshVar
  unify t1 (TArray interval v NoLoc) (locOf e1)
  return v
infer (ArrUpd e1 e2 e3 _) = do
  t1 <- infer e1
  let interval = case t1 of
        TArray itv _ _ -> itv
        _              -> emptyInterval
  t2 <- infer e2
  t3 <- infer e3
  unify t2 (TBase TInt NoLoc)         (locOf e2)
  unify t1 (TArray interval t3 NoLoc) (locOf e1)
  return t1
infer (Case expr cs _) = do
  te <- infer expr
  ts <- mapM (inferCaseConstructor te) cs
  t  <- freshVar
  mapM_ (uncurry (unify t)) (zip ts (map locOf cs))
  return t
 where
  inferCaseConstructor t (CaseConstructor n patts e) = do
    subs <- inferPatts t n patts
    inferInEnv subs (infer e)

  inferPatts :: Type -> Name -> [Pattern] -> Infer [(Name, Type)]
  inferPatts t n patts = do
    tn             <- lookupInferEnv n
    (subs, tpatts) <- unzip <$> mapM inferPatt patts
    let subs' = concat subs
    let ns    = map fst subs'

    -- check if there is duplicated identifier
    let dups = map head . filter ((> 1) . length) . group $ ns
    unless (null dups) $ throwError $ DuplicatedIdentifier
      (head dups)
      (locOf . head $ dups)

    unify tn (wrapTFunc tpatts t) (locOf tpatts)
    return subs'

  inferPatt :: Pattern -> Infer ([(Name, Type)], Type)
  inferPatt (PattLit    x) = return ([], litTypes x (locOf x))
  inferPatt (PattBinder n) = do
    tn <- freshVar
    return ([(n, tn)], tn)
  inferPatt (PattWildcard _         ) = ([], ) <$> freshVar
  inferPatt (PattConstructor n patts) = do
    tpatts <- freshVar
    inferPatts tpatts n patts <&> (, tpatts)

emptyInterval :: Interval
emptyInterval = Interval (Including zero) (Excluding zero) NoLoc
  where zero = Lit (Num 0) NoLoc

runInfer :: Environment -> Infer Type -> TM (Type, Subs Type)
runInfer env m = do
  (t, cs) <- toEvalStateT env m
  s       <- solveConstraints cs
  return (subst s t, s)

inferExpr' :: Environment -> Expr -> TM (Type, Subs Type)
inferExpr' env = runInfer env . infer

inferExpr :: Environment -> Expr -> TM Type
inferExpr = curry (fmap fst . uncurry inferExpr')

lookupInferEnv :: Name -> Infer Type
lookupInferEnv n = do
  env <- ask
  case Map.lookup n (envLocalDefns env) of
    Nothing -> case Map.lookup n (envLocalContext env) of
      Nothing         -> throwError $ NotInScope n (locOf n)
      Just []         -> throwError $ NotInScope n (locOf n) -- when there are no clauses in a function
      Just (expr : _) -> infer expr
    Just t -> return t

freshVar :: Infer Type
freshVar = do
  TMetaVar <$> freshName "Type.metaVar" NoLoc

------------------------------------------
-- type check
------------------------------------------

checkAssign :: Environment -> (Name, Expr) -> TM ()
checkAssign env (n, expr) = case Map.lookup n (envLocalDefns env) of
  Nothing -> throwError $ NotInScope n (locOf n)
  Just t  -> do
    checkIsType env expr t

checkIsType :: Environment -> Expr -> Type -> TM ()
checkIsType env expr t = do
  eType <- inferExpr env expr
  void $ unifies eType t (locOf eType)

checkPredicate :: Environment -> Expr -> TM ()
checkPredicate env p = checkIsType env p (TBase TBool NoLoc)

checkType :: Environment -> Type -> TM ()
checkType _   (TBase _ _                    ) = return ()
checkType env (TArray (Interval e1 e2 _) t _) = do
  checkIsType env (getEndpointExpr e1) (TBase TInt NoLoc)
  checkIsType env (getEndpointExpr e2) (TBase TInt NoLoc)
  checkType env t
 where
  getEndpointExpr (Including e) = e
  getEndpointExpr (Excluding e) = e
checkType env (  TFunc t1 t2 _) = checkType env t1 >> checkType env t2
checkType env t@(TCon  n  _  _) = do
  maybe (throwError $ UndefinedType n (locOf t))
        (\_ -> return ())
        (Map.lookup n (envTypeDefns env))
checkType _ (TVar _ _  ) = return ()
checkType _ (TMetaVar _) = return ()

checkGdCmd :: Environment -> GdCmd -> TM ()
checkGdCmd env (GdCmd expr stmts _) = do
  checkPredicate env expr
  mapM_ (checkStmt env) stmts

checkStmt :: Environment -> Stmt -> TM ()
checkStmt _ (Skip  _) = return ()
checkStmt _ (Abort _) = return ()
checkStmt env (Assign ns es loc)
  | -- NOTE : Not sure if Assign work this way
    length ns > length es
  = let extraVars = drop (length es) ns
    in  throwError $ NotEnoughExprsInAssigment (NE.fromList extraVars) loc
  | length ns < length es
  = let extraExprs = drop (length ns) es
    in  throwError $ TooManyExprsInAssigment (NE.fromList extraExprs) loc
  | otherwise
  = forM_ (zip ns es) (checkAssign env)
checkStmt env (AAssign x i e _) = do
  checkIsType env i (tInt NoLoc)
  te <- inferExpr env e
  checkIsType
    env
    x
    (TArray (Interval (Including i) (Including i) (locOf i)) te (locOf x))
checkStmt env (Assert expr _) = do
  checkPredicate env expr
checkStmt env (LoopInvariant e1 e2 _) = do
  checkPredicate env e1
  checkIsType env e2 (TBase TInt NoLoc)
checkStmt env (Do    gdcmds _) = mapM_ (checkGdCmd env) gdcmds
checkStmt env (If    gdcmds _) = mapM_ (checkGdCmd env) gdcmds
checkStmt _   (Spec  _      _) = return ()
checkStmt _   (Proof _      _) = return ()
checkStmt env (Alloc x es _  ) = case Map.lookup x (envLocalDefns env) of
  Nothing -> throwError $ NotInScope x (locOf x)
  Just t  -> mapM_ (\e -> checkIsType env e t) es
checkStmt env (HLookup x e _) = case Map.lookup x (envLocalDefns env) of
  Nothing -> throwError $ NotInScope x (locOf x)
  Just t  -> checkIsType env e t
checkStmt env (HMutate e1 e2 _) = do
  inferExpr env e1 >>= checkIsType env e2
checkStmt env (Dispose e _) = checkIsType env e (tInt NoLoc)
checkStmt env (Block   p _) = checkProg env p

-- NOTE : should this be check here?
checkIsVarAssign :: [Declaration] -> Stmt -> TM ()
checkIsVarAssign declarations (Assign ns _ _) =
  let (_, cs) = splitDecls declarations
  in  forM_
        ns
        (\n -> when (n `elem` cs) $ throwError $ AssignToConst n (locOf n))
 where
  splitDecls [] = ([], [])
  splitDecls (d : ds) =
    let (vs, cs) = splitDecls ds
    in  case d of
          VarDecl   n _ _ _ -> (n ++ vs, cs)
          ConstDecl n _ _ _ -> (vs, n ++ cs)
checkIsVarAssign _ _ = return ()

checkEnvironment :: Environment -> TM ()
checkEnvironment env = do
  -- check type declarations
  forM_ (envTypeDefns env) checkTypeDefn

  -- check local declaration type
  mapM_ (checkType env) (envLocalDefns env)
 where
  checkTypeDefn (binders, qdcons) = do
    checkQTyConArg binders
    mapM_ (checkTypeDefnCtor binders) qdcons

  checkQTyConArg []       = return ()
  checkQTyConArg (x : xs) = if x `elem` xs
    then throwError $ DuplicatedIdentifier x (locOf x)
    else checkQTyConArg xs

  checkTypeDefnCtor binders (TypeDefnCtor _ ts) = do
    forM_
      (fv ts)
      (\n -> if n `Set.member` Set.fromList binders
        then return ()
        else throwError $ NotInScope n (locOf n)
      )

checkProg :: Environment -> Program -> TM ()
checkProg env (Program defns decls props stmts _) = do
  mapM_ (checkIsVarAssign decls) stmts
  env' <- extendEnv env =<< defnsAndDeclsToEnv defns decls
  checkEnvironment env'
  mapM_ (inferExpr env') props
  mapM_ (checkStmt env') stmts

defnsAndDeclsToEnv :: Definitions -> [Declaration] -> TM Environment
defnsAndDeclsToEnv defns decls = do
  -- add type declaration and defnFuncs to enviornment
  env <- foldM addTypeDecl mempty (defnTypes defns)
    >>= \tds -> return $ Environment mempty tds (defnFuncs defns)

  -- add var const declaration into enviornment, add type inference result of defns into enviornment
  foldM addDecl env decls >>= inferLocalContext
 where
  addTypeDecl env (TypeDefn name binders qdcons _) = if name `Map.member` env
    then throwError $ DuplicatedIdentifier name (locOf name)
    else return $ Map.insert name (binders, qdcons) env

  addDecl env (ConstDecl ns t _ _) = foldM (addSingleDecl t) env ns
  addDecl env (VarDecl   ns t _ _) = foldM (addSingleDecl t) env ns

  addSingleDecl t env n =
    env `extendEnv` Environment (Map.singleton n t) mempty mempty

  inferLocalContext env = do
    Map.foldlWithKey
      (\envM name exprs -> do
        env' <- envM
        -- banacorn: there maybe be more than one clauses in the definition of a function
        -- we use the first clause to infer the type of the function
        case Maybe.listToMaybe exprs of
          Nothing   -> return env'
          Just expr -> do
            (eType, subs) <- inferExpr' env' expr
            lds           <-
              Map.map (subst subs)
                <$> (envLocalDefns env' `extendType` (name, eType, locOf expr))
            return $ env' { envLocalDefns = lds }
      )
      (pure env)
      (envLocalContext env)

checkProgram :: Program -> TM ()
checkProgram = checkProg mempty

runTM' :: TM a -> Except TypeError a
runTM' p = evalStateT p initFreshState

runTM :: TM a -> Either TypeError a
runTM = runExcept . runTM'

------------------------------------------
-- unification
------------------------------------------

type Constraint = (Type, Type, Loc)

type Unifier = (Subs Type, [Constraint])

emptyUnifier :: Unifier
emptyUnifier = (emptySubs, [])

unifies :: Type -> Type -> Loc -> TM (Subs Type)
unifies (TBase t1 _) (TBase t2 _) _ | t1 == t2 = return emptySubs
unifies (TArray _ t1 _) (TArray _ t2 _) l = unifies t1 t2 l  {-  | i1 == i2 = unifies t1 t2 -}
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

solveConstraints :: [Constraint] -> TM (Subs Type)
solveConstraints cs = solveUnifier (emptySubs, cs)

solveUnifier :: Unifier -> TM (Subs Type)
solveUnifier (s, []              ) = return s
solveUnifier (s, (t1, t2, l) : cs) = do
  su <- unifies t1 t2 l
  solveUnifier (su `compose` s, map (f (subst su)) cs)
  where f g (a, b, c) = (g a, g b, c)

bind :: Name -> Type -> Loc -> TM (Subs Type)
bind x t l | occurs x t = throwError $ RecursiveType x t l
           | otherwise  = return (Map.singleton x t)

------------------------------------------
-- helper functions
------------------------------------------

litTypes :: Lit -> Loc -> Type
litTypes (Num _) l = TBase TInt l
litTypes (Bol _) l = TBase TBool l
litTypes (Chr _) l = TBase TChar l
litTypes Emp     l = TBase TBool l

-- NOTE : EQ, NEQ, NEQU is redundant here
inferOpTypes :: Op -> Infer Type
inferOpTypes op = do
  case op of
    ChainOp (EQ   l) -> f l
    ChainOp (NEQ  l) -> f l
    ChainOp (NEQU l) -> f l
    _                -> return (opTypes op)
 where
  f l = do
    x <- freshVar
    return (TFunc x (TFunc x (TBase TBool l) l) l)

tBool, tInt :: Loc -> Type
tBool = TBase TBool
tInt = TBase TInt

(.->) :: (Loc -> Type) -> (Loc -> Type) -> (Loc -> Type)
(t1 .-> t2) l = TFunc (t1 l) (t2 l) l
infixr 1 .->

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


opTypes :: Op -> Type
opTypes (ChainOp op) = chainOpTypes op
opTypes (ArithOp op) = arithOpTypes op
