{-# LANGUAGE MultiWayIf #-}
module GCL.Scope where

import           Syntax.Abstract
import           Syntax.Abstract.Util
import           Syntax.Common
import           Server.TokenMap
import           Control.Monad.Except
import           Control.Monad.RWS
import           Data.List
import           Data.Loc                       ( Loc
                                                , (<-->)
                                                , Located(locOf)
                                                )
import qualified Data.Map                      as Map
import           Data.Text                      ( Text )


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
    TypeDefnInfo [Name] Loc
    | TypeDefnCtorInfo Type Loc
    | TypeInfo Type Loc
    deriving (Show)

instance Eq TypeInfo where
  TypeDefnInfo{}     == TypeDefnCtorInfo{} = False
  TypeDefnCtorInfo{} == TypeDefnInfo{}     = False
  _                  == _                  = True

instance Located TypeInfo where
  locOf (TypeDefnInfo     _ l) = l
  locOf (TypeDefnCtorInfo _ l) = l
  locOf (TypeInfo         _ l) = l

class CollectIds a where
    collectIds :: a -> [(Text, TypeInfo)]

instance (CollectIds a) => CollectIds [a] where
  collectIds xs = concatMap collectIds xs

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
            in  tdInfo : tysInfo

  collectIds (FuncDefnSig (Name n l) t _ _) = [(n, TypeInfo t l)]
  collectIds _                              = []

instance CollectIds Declaration where
  collectIds (ConstDecl ns t _ _) = map (\(Name n l) -> (n, TypeInfo t l)) ns
  collectIds (VarDecl   ns t _ _) = map (\(Name n l) -> (n, TypeInfo t l)) ns

instance CollectIds Stmt where
  collectIds (Block prog _) = collectIds prog
  collectIds _              = []

instance CollectIds Pattern where
  collectIds PattLit{}                 = []
  collectIds (PattBinder n) = [(nameToText n, TypeInfo (TMetaVar n) (locOf n))]
  collectIds PattWildcard{}            = []
  collectIds (PattConstructor _ patts) = collectIds patts

type ScopeM = M TypeInfo ()
type ScopeCheckM = ExceptT ScopeError ScopeM

runScopeCheckM :: Program -> (Either ScopeError (), TokenMap ())
runScopeCheckM prog =
  let (err, _, w) = runRWS (runExceptT . scopeCheck $ prog) [] () in (err, w)

dups :: Eq a => [a] -> [a]
dups = map head . filter ((> 1) . length) . group

duplicationCheck :: [(Text, TypeInfo)] -> ScopeCheckM (Scope TypeInfo)
duplicationCheck ns =
  let ds = dups ns
  in  if null ds
        then return . Map.fromList $ ns
        else throwError
          $ DuplicatedIdentifiers (map (\(t, info) -> Name t (locOf info)) ds)

generateScope :: Program -> ScopeCheckM (Scope TypeInfo)
generateScope = duplicationCheck . collectIds

class ScopeCheckable a where
    scopeCheck :: a -> ScopeCheckM ()

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
    info <- lift . lookupScopes . nameToText $ n
    case info of
      (Just (TypeDefnInfo _ _)) -> return ()
      _                         -> throwError . NotInScope $ n
  scopeCheck (TVar _ _  ) = return ()
  scopeCheck (TMetaVar _) = return ()

instance ScopeCheckable Expr where
  scopeCheck Lit{}     = return ()
  scopeCheck (Var n _) = do
    info <- lift . lookupScopes . nameToText $ n
    case info of
      (Just TypeInfo{}        ) -> return ()
      (Just TypeDefnCtorInfo{}) -> return ()
      _                         -> throwError . NotInScope $ n
  scopeCheck (Const n _) = do
    info <- lift . lookupScopes . nameToText $ n
    case info of
      (Just TypeInfo{}        ) -> return ()
      (Just TypeDefnCtorInfo{}) -> return ()
      _                         -> throwError . NotInScope $ n
  scopeCheck Op{}        = return ()
  scopeCheck (App a b _) = scopeCheck a >> scopeCheck b
  scopeCheck (Lam x e _) = localScope
    (Map.singleton (nameToText x) (TypeInfo (TMetaVar x) (locOf x)))
    (scopeCheck e)
  scopeCheck (Quant op ids rng body _) = do
    scopeCheck op
    s <-
      duplicationCheck
      . map (\n -> (nameToText n, TypeInfo (TMetaVar n) (locOf n)))
      $ ids
    localScope s (scopeCheck rng)
    localScope s (scopeCheck body)
  scopeCheck RedexStem{}      = return ()
  scopeCheck Redex{}          = return ()
  scopeCheck (ArrIdx e1 e2 _) = scopeCheck e1 >> scopeCheck e2
  scopeCheck (ArrUpd e1 e2 e3 _) =
    scopeCheck e1 >> scopeCheck e2 >> scopeCheck e3
  scopeCheck (Case e ctors _) = scopeCheck e >> scopeCheck ctors

instance ScopeCheckable CaseConstructor where
  scopeCheck (CaseConstructor patt body) = do
    scopeCheck patt
    s <- duplicationCheck . collectIds $ patt
    localScope s (scopeCheck body)

instance ScopeCheckable Pattern where
  scopeCheck PattLit{}                 = return ()
  scopeCheck PattBinder{}              = return ()
  scopeCheck PattWildcard{}            = return ()
  scopeCheck (PattConstructor n patts) = do
    info <- lift . lookupScopes . nameToText $ n
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
    info <- lift . lookupScopes . nameToText $ n
    case info of
      Just TypeDefnCtorInfo{} -> return ()
      Just TypeInfo{}         -> return ()
      _                       -> throwError $ NotInScope n
