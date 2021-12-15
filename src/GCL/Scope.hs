{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}

module GCL.Scope where

import           Syntax.Abstract
import           Syntax.Abstract.Util
import           Syntax.Common
import           Server.TokenMap                ( Scope )
--import qualified Server.TokenMap               as TokenMap
import           Control.Monad.Except
import           Control.Monad.Reader
import           Data.List
--import           Data.Loc.Range                 ( fromLoc )
import           Data.Loc                       ( Loc
                                                , (<-->)
                                                , Located(locOf)
                                                )
import qualified Data.List                     as List
import qualified Data.Map                      as Map
import           Data.Text                      ( Text )
import           Data.Bifunctor                 ( bimap
                                                , first
                                                )


data ScopeError =
    NotInScope Name
    | DuplicatedIdentifiers [Name]
    | RedundantNames [Name]
    | RedundantPatterns [Pattern]
    | RedundantExprs [Expr]
    deriving (Show, Eq)

instance Located ScopeError where
  locOf (NotInScope            n    ) = locOf n
  locOf (DuplicatedIdentifiers ns   ) = locOf ns
  locOf (RedundantNames        ns   ) = locOf ns
  locOf (RedundantPatterns     patts) = locOf patts
  locOf (RedundantExprs        exprs) = locOf exprs

data TypeInfo =
    TypeDefnCtorInfo Type Loc
    | FuncDefnInfo [Expr] (Maybe Type) Loc
    | ConstTypeInfo Type Loc
    | VarTypeInfo Type Loc
    deriving (Eq, Show)

data TypeDefnInfo = TypeDefnInfo [Name] Loc
  deriving (Eq, Show)

instance Located TypeInfo where
  locOf (TypeDefnCtorInfo _ l) = l
  locOf (FuncDefnInfo _ _ l  ) = l
  locOf (ConstTypeInfo _ l   ) = l
  locOf (VarTypeInfo   _ l   ) = l
instance Located TypeDefnInfo where
  locOf (TypeDefnInfo _ l) = l

class CollectIds a where
    collectIds :: a -> ([(Text, TypeDefnInfo)], [(Text, TypeInfo)])

instance (CollectIds a) => CollectIds [a] where
  collectIds xs =
    foldl (\(ms, ns) (x, y) -> (ms ++ x, ns ++ y)) ([], []) (map collectIds xs)

instance CollectIds Program where
  collectIds (Program defns decls _ _ _) = collectIds defns <> collectIds decls

instance CollectIds Definition where
  collectIds (TypeDefn n@(Name nt nl) args tys _) =
    let t = TCon n args (n <--> args)
    in  let tdInfo = (nt, TypeDefnInfo args nl)
        in  let tysInfo = map
                  (\(TypeDefnCtor (Name ct cl) ts) ->
                    (ct, TypeDefnCtorInfo (wrapTFunc ts t) cl)
                  )
                  tys
            in  ([tdInfo], tysInfo)

  collectIds (FuncDefnSig (Name n l) t _ _) =
    ([], [(n, FuncDefnInfo [] (Just t) l)])
  collectIds (FuncDefn (Name n l) exprs) =
    ([], [(n, FuncDefnInfo exprs Nothing l)])

instance CollectIds Declaration where
  collectIds (ConstDecl ns t _ _) =
    ([], map (\(Name n l) -> (n, ConstTypeInfo t l)) ns)
  collectIds (VarDecl ns t _ _) =
    ([], map (\(Name n l) -> (n, VarTypeInfo t l)) ns)

instance CollectIds Stmt where
  collectIds (Block prog _) = collectIds prog
  collectIds _              = ([], [])

instance CollectIds Pattern where
  collectIds PattLit{} = ([], [])
  collectIds (PattBinder n) =
    ([], [(nameToText n, VarTypeInfo (TMetaVar n) (locOf n))])
  collectIds PattWildcard{}            = ([], [])
  collectIds (PattConstructor _ patts) = collectIds patts

type ScopeM a
  = forall m . (MonadReader (Scope TypeDefnInfo, Scope TypeInfo) m) => m a
type ScopeCheckM a
  =  forall m
   . ( MonadReader (Scope TypeDefnInfo, Scope TypeInfo) m
     , MonadError ScopeError m
     )
  => m a

runScopeCheckM :: Program -> Either ScopeError ()
runScopeCheckM prog =
  runReader (runExceptT . scopeCheck $ prog) (mempty, mempty)

dups :: Eq a => [a] -> [a]
dups = map head . filter ((> 1) . length) . group

duplicationCheck
  :: (Eq a, Located a, MonadError ScopeError m) => [(Text, a)] -> m [(Text, a)]
duplicationCheck ns =
  let ds = dups ns
  in  if null ds
        then return ns
        else throwError
          $ DuplicatedIdentifiers (map (\(t, info) -> Name t (locOf info)) ds)

generateScope :: Program -> ScopeCheckM (Scope TypeDefnInfo, Scope TypeInfo)
generateScope prog = do
  let (tids, ids) = collectIds prog


  (s1, s2) <- (,) <$> duplicationCheck tids <*> duplicationCheck ids
  --foldM (\env' (t, info) ->
        --case info of
            --_ -> _
    --) mempty s
  --mapM_
    --(\(n, t) -> case fromLoc (locOf t) of
      --Nothing  -> return ()
      --Just rng -> tell $ TokenMap.singleton rng (n, t)
    --)
    --ids
  return (Map.fromList s1, Map.fromList s2)

class ScopeCheckable a where
    scopeCheck :: a -> ScopeCheckM ()

localScope
  :: (Scope TypeDefnInfo, Scope TypeInfo) -> ScopeCheckM () -> ScopeCheckM ()
localScope (s1, s2) = local (bimap (Map.union s1) (Map.union s2))

lookupFst :: (MonadReader (Scope a, Scope b) m) => Name -> m (Maybe a)
lookupFst n = asks (lookupScope n . fst)

lookupSnd :: (MonadReader (Scope a, Scope b) m) => Name -> m (Maybe b)
lookupSnd n = asks (lookupScope n . snd)

lookupScope :: Name -> Scope a -> Maybe a
lookupScope n = Map.lookup (nameToText n)

instance ScopeCheckable a => ScopeCheckable [a] where
  scopeCheck xs = mapM_ scopeCheck xs

instance ScopeCheckable Program where
  scopeCheck prog@(Program _ _ props stmts _) = do
    s <- generateScope prog
    localScope s (scopeCheck props)
    localScope s (scopeCheck stmts)

instance ScopeCheckable Stmt where
  scopeCheck Skip{}           = return ()
  scopeCheck Abort{}          = return ()
  scopeCheck (Assign ns es _) = do
    scopeCheck ns
    scopeCheck es
    let an = length (zip ns es)
    if
      | an < length ns -> throwError . RedundantNames $ ns
      | an < length es -> throwError . RedundantExprs $ es
      | otherwise      -> return ()
  scopeCheck (AAssign e1 e2 e3 _) =
    scopeCheck e1 >> scopeCheck e2 >> scopeCheck e3
  scopeCheck (Assert e _           ) = scopeCheck e
  scopeCheck (LoopInvariant e1 e2 _) = scopeCheck e1 >> scopeCheck e2
  scopeCheck (Do gds _             ) = scopeCheck gds
  scopeCheck (If gds _             ) = scopeCheck gds
  scopeCheck Spec{}                  = return ()
  scopeCheck Proof{}                 = return ()
  scopeCheck (Alloc   n  es _)       = scopeCheck n >> scopeCheck es
  scopeCheck (HLookup n  e  _)       = scopeCheck n >> scopeCheck e
  scopeCheck (HMutate e1 e2 _)       = scopeCheck e1 >> scopeCheck e2
  scopeCheck (Dispose e    _ )       = scopeCheck e
  scopeCheck (Block   prog _ )       = scopeCheck prog

instance ScopeCheckable GdCmd where
  scopeCheck (GdCmd e stmts _) = scopeCheck e >> scopeCheck stmts

instance ScopeCheckable Endpoint where
  scopeCheck (Including e) = scopeCheck e
  scopeCheck (Excluding e) = scopeCheck e

instance ScopeCheckable Interval where
  scopeCheck (Interval e1 e2 _) = scopeCheck e1 >> scopeCheck e2

instance ScopeCheckable Type where
  scopeCheck TBase{}          = return ()
  scopeCheck (TArray i  t  _) = scopeCheck i >> scopeCheck t
  scopeCheck (TFunc  t1 t2 _) = scopeCheck t1 >> scopeCheck t2
  scopeCheck (TCon   n  _  _) = do
    info <- lookupFst n
    case info of
      Just TypeDefnInfo{} -> return ()
      _                   -> throwError $ NotInScope n
  scopeCheck (TVar _ _  ) = return ()
  scopeCheck (TMetaVar _) = return ()

instance ScopeCheckable Expr where
  scopeCheck Lit{}       = return ()
  scopeCheck (Var   n _) = scopeCheck n
  scopeCheck (Const n _) = scopeCheck n
  scopeCheck Op{}        = return ()
  scopeCheck (App a b _) = scopeCheck a >> scopeCheck b
  scopeCheck (Lam x e _) = localScope
    (mempty, Map.singleton (nameToText x) (VarTypeInfo (TMetaVar x) (locOf x)))
    (scopeCheck e)
  scopeCheck (Quant op ids rng body _) = do
    scopeCheck op
    s <-
      fmap Map.fromList
      . duplicationCheck
      . map (\n -> (nameToText n, VarTypeInfo (TMetaVar n) (locOf n)))
      $ ids
    localScope (mempty, s) (scopeCheck rng)
    localScope (mempty, s) (scopeCheck body)
  scopeCheck RedexStem{}      = return ()
  scopeCheck Redex{}          = return ()
  scopeCheck (ArrIdx e1 e2 _) = scopeCheck e1 >> scopeCheck e2
  scopeCheck (ArrUpd e1 e2 e3 _) =
    scopeCheck e1 >> scopeCheck e2 >> scopeCheck e3
  scopeCheck (Case e ctors _) = scopeCheck e >> scopeCheck ctors

instance ScopeCheckable CaseConstructor where
  scopeCheck (CaseConstructor patt body) = do
    scopeCheck patt
    let (tids, ids) = collectIds patt
    s <-
      (,)
      <$> (Map.fromList <$> duplicationCheck tids)
      <*> (Map.fromList <$> duplicationCheck ids)
    localScope s (scopeCheck body)

instance ScopeCheckable Pattern where
  scopeCheck PattLit{}                 = return ()
  scopeCheck PattBinder{}              = return ()
  scopeCheck PattWildcard{}            = return ()
  scopeCheck (PattConstructor n patts) = do
    info <- lookupFst n
    case info of
      Just (TypeDefnInfo args _) -> do
        let an = length (zip args patts)
        if
          | an < length args
          -> throwError . RedundantNames . drop an $ args
          | an < length patts
          -> throwError . RedundantPatterns . drop an $ patts
          | otherwise
          -> scopeCheck patts
      _ -> throwError $ NotInScope n
instance ScopeCheckable Name where
  scopeCheck n = do
    void $ lookupSnd n
