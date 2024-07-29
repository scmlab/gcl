{-# LANGUAGE FlexibleInstances, UndecidableInstances,
             MultiParamTypeClasses, FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DeriveGeneric #-}
module GCL.Common where

import           Control.Monad.Except
import           Control.Monad.RWS              ( RWST(..), MonadState (get, put) )
import           Control.Monad.State            ( StateT(..) )
import           Data.Aeson                     ( ToJSON )
import           Data.Map                       ( Map )
import qualified Data.Map                      as Map
import           Data.Set                       ( Set
                                                , (\\)
                                                )
import qualified Data.Set                      as Set
import           Data.Text                      ( Text )
import qualified Data.Text                     as Text
import           Data.Loc.Range                 ( Range )
import           Data.Loc                       ( Loc(..)
                                                , Located
                                                , locOf
                                                )
import           GHC.Generics
import           Syntax.Abstract
import           Syntax.Common.Types
import qualified Syntax.Typed                  as Typed


data Index = Index Name | Hole Range deriving (Eq, Show, Ord)

type TypeEnv = [(Index, Type)]

type KindEnv = [KindItem]

data KindItem
  = KindAnno Name Kind
  | UnsolvedUni Name
  | SolvedUni Name Kind
  deriving (Show)

-- get a fresh variable
class Monad m => Fresh m where
    fresh :: m Text
    fresh = freshPre (Text.pack "")

    freshPre :: Text -> m Text

freshName :: Fresh m => Text -> Loc -> m Name
freshName prefix l = Name <$> freshPre prefix <*> pure l

freshName' :: Fresh m => Text -> m Name
freshName' prefix = freshName prefix NoLoc

class Counterous m where
  countUp :: m Int

instance {-# OVERLAPPABLE #-}
         (Monad m, Counterous m) => Fresh m where
  fresh = Text.pack . ("?m_" ++) . show <$> countUp
  freshPre prefix =
      Text.pack . ("?" ++) . (Text.unpack prefix ++) .
           ("_" ++) . show <$> countUp

type FreshState = Int

initFreshState :: FreshState
initFreshState = 0

type Subs a = Map Name a
type Env a = Map Name a

emptySubs :: Subs a
emptySubs = mempty

emptyEnv :: Env a
emptyEnv = mempty

freeMetaVars :: Type -> Set Name
freeMetaVars (TBase _ _   ) = mempty
freeMetaVars (TArray _ t _) = freeMetaVars t
freeMetaVars (TTuple _    ) = mempty
freeMetaVars (TOp _       ) = mempty
freeMetaVars (TData _ _   ) = mempty
freeMetaVars (TApp l r _  ) = freeMetaVars l <> freeMetaVars r
freeMetaVars (TVar _ _    ) = mempty
freeMetaVars (TMetaVar n _) = Set.singleton n

-- A class of types for which we may compute their free variables.
class Free a where
  freeVars :: a -> Set Name
  freeVarsT :: a -> Set Text

  freeVarsT = Set.map nameToText . freeVars

occurs :: Free a => Name -> a -> Bool
occurs n x = n `Set.member` freeVars x

instance Free a => Free (Subs a) where
  freeVars = Set.unions . Map.map freeVars


instance {-# OVERLAPPABLE #-} Free a => Free [a] where
  freeVars l = foldMap freeVars l

instance (Free a, Free b) => Free (a, b) where
  freeVars (x,y) = freeVars x <> freeVars y

instance (Free a, Free b, Free c) => Free (a, b, c) where
  freeVars (x,y,z) = freeVars x <> freeVars y <> freeVars z

instance Free a => Free (Maybe a) where
  freeVars = maybe mempty freeVars

instance Free Type where
  freeVars (TBase _ _   ) = mempty
  freeVars (TArray _ t _) = freeVars t
  freeVars (TTuple _    ) = mempty
  freeVars (TData _ _   ) = mempty
  freeVars (TOp _       ) = mempty
  freeVars (TApp l r _  ) = freeVars l <> freeVars r
  freeVars (TVar x _    ) = Set.singleton x
  freeVars (TMetaVar n _) = Set.singleton n

instance {-# OVERLAPS #-} Free TypeEnv where
  freeVars env = foldMap freeVars $ Map.elems $ Map.fromList env

instance Free Expr where
  freeVars (Var   x _            ) = Set.singleton x
  freeVars (Const x _            ) = Set.singleton x
  freeVars (Op _                 ) = mempty
  freeVars (Chain chain          ) = freeVars chain
  freeVars (Lit _ _              ) = mempty
  freeVars (App  e1 e2      _    ) = freeVars e1 <> freeVars e2
  freeVars (Func _  clauses _    ) = Set.unions (fmap freeVars clauses)
  freeVars (Lam  x  e       _    ) = freeVars e \\ Set.singleton x
  freeVars (Tuple xs             ) = Set.unions (map freeVars xs)
  freeVars (Quant op xs range term _) =
    (freeVars op <> freeVars range <> freeVars term) \\ Set.fromList xs
  freeVars (RedexKernel _ _ fv _) = fv
  freeVars (RedexShell _ e      ) = freeVars e
  freeVars (ArrIdx e1 e2 _      ) = freeVars e1 <> freeVars e2
  freeVars (ArrUpd e1 e2 e3 _   ) = freeVars e1 <> freeVars e2 <> freeVars e3
  freeVars (Case e clauses _    ) = freeVars e <> Set.unions (map freeVars clauses)


instance Free Chain where
  freeVars (Pure expr _) = freeVars expr
  freeVars (More chain _op expr _) = freeVars chain <> freeVars expr

instance Free FuncClause where
  freeVars (FuncClause patterns expr) = freeVars expr \\ Set.unions (map freeVars patterns)

instance Free CaseClause where
  freeVars (CaseClause patt expr) = freeVars expr \\ freeVars patt

instance Free Pattern where
  freeVars (PattLit      _      ) = mempty
  freeVars (PattBinder   n      ) = Set.singleton n
  freeVars (PattWildcard _      ) = mempty
  freeVars (PattConstructor _ ps) = foldMap freeVars ps

instance Free Declaration where
  freeVars (ConstDecl ns t expr _) =
    Set.fromList ns <> freeVars t <> freeVars expr
  freeVars (VarDecl   ns t expr _) =
    Set.fromList ns <> freeVars t <> freeVars expr

instance Free Stmt where
  freeVars (Skip  _) = mempty
  freeVars (Abort _) = mempty
  freeVars (Assign ns es _) =
    Set.fromList ns <> Set.unions (map freeVars es)
  freeVars (AAssign a i e _) =
    freeVars a <> freeVars i <> freeVars e
  freeVars (Assert p _) = freeVars p
  freeVars (LoopInvariant p b _) = freeVars p <> freeVars b
  freeVars (Do gdcmds _) = Set.unions (map freeVars gdcmds)
  freeVars (If gdcmds _) = Set.unions (map freeVars gdcmds)
  freeVars (Spec  _ _) = mempty
  freeVars Proof {} = mempty
  freeVars (Alloc x es _) =
     Set.singleton x <> Set.unions (map freeVars es)
  freeVars (HLookup x e _) =
     Set.singleton x <> freeVars e
  freeVars (HMutate e1 e2 _) = freeVars e1 <> freeVars e2
  freeVars (Dispose e _) = freeVars e
  freeVars (Block prog _) = freeVars prog

instance Free GdCmd where
  freeVars (GdCmd g stmts _) =
    freeVars g <> Set.unions (map freeVars stmts)

instance Free Program where
  freeVars (Program _defns decls props stmts _) =
    foldMap freeVars decls <> foldMap freeVars props <> foldMap freeVars stmts
   -- SCM: TODO: deal with defns later.

toStateT :: Monad m => r -> RWST r w s m a -> StateT s m a
toStateT r m = StateT
  (\s -> do
    (a, s', _) <- runRWST m r s
    return (a, s')
  )

toEvalStateT :: Monad m => r -> RWST r w s m a -> StateT s m (a, w)
toEvalStateT r m = StateT
  (\s -> do
    (a, s', w) <- runRWST m r s
    return ((a, w), s')
  )

--------------------------------------------------------------------------------
-- The elaboration monad

type ElaboratorM = StateT (FreshState, [(Index, Kind)], [(Index, TypeInfo)]) (Except TypeError)

instance Counterous ElaboratorM where
  countUp = do
    (count, typeDefnInfo, typeInfo) <- get
    put (succ count, typeDefnInfo, typeInfo)
    return count

runElaboration
  :: Elab a => a -> Either TypeError (Typed a)
runElaboration a = do
  ((_, elaborated, _), _state) <- runExcept (runStateT (elaborate a mempty) (0, mempty, mempty))
  Right elaborated

data TypeError
    = NotInScope Name
    | UnifyFailed Type Type Loc
    | KindUnifyFailed Kind Kind Loc -- TODO: Deal with this replication in a better way.
    | NotKFunc Kind Loc
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
  locOf (UnifyFailed _ _ l          ) = l
  locOf (KindUnifyFailed _ _ l      ) = l
  locOf (NotKFunc _ l               ) = l
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

--------------------------------------------------------------------------------
-- Elaboration

-- The type family `Typed` turns data into its typed version.
type family Typed untyped where
  Typed Definition = Typed.TypedDefinition
  Typed Declaration = Typed.TypedDeclaration
  Typed TypeDefnCtor = Typed.TypedTypeDefnCtor
  Typed Program = Typed.TypedProgram
  Typed Stmt = Typed.TypedStmt
  Typed GdCmd = Typed.TypedGdCmd
  Typed Expr = Typed.TypedExpr
  Typed Chain = Typed.TypedChain
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
    elaborate :: a -> TypeEnv -> ElaboratorM (Maybe Type, Typed a, Subs Type)

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
  subst _ (TOp op        ) = TOp op
  subst s (TApp l r loc  ) = TApp (subst s l) (subst s r) loc
  subst _ t@TData{}        = t
  subst s t@(TVar n _    ) = Map.findWithDefault t n s
  subst s t@(TMetaVar n _) = Map.findWithDefault t n s

instance Substitutable (Subs Type) Typed.TypedExpr where
  subst s (Typed.Lit lit ty loc) = Typed.Lit lit (subst s ty) loc
  subst s (Typed.Var name ty loc) = Typed.Var name (subst s ty) loc
  subst s (Typed.Const name ty loc) = Typed.Const name (subst s ty) loc
  subst s (Typed.Op op ty) = Typed.Op op $ subst s ty
  subst s (Typed.Chain ch) = Typed.Chain $ subst s ch
  subst s (Typed.App e1 e2 loc) = Typed.App (subst s e1) (subst s e2) loc
  subst s (Typed.Lam name ty expr loc) = Typed.Lam name (subst s ty) (subst s expr) loc
  subst s (Typed.Quant quantifier vars restriction inner loc) = Typed.Quant (subst s quantifier) vars (subst s restriction) (subst s inner) loc
  subst s (Typed.ArrIdx arr index loc) = Typed.ArrIdx (subst s arr) (subst s index) loc
  subst s (Typed.ArrUpd arr index expr loc) = Typed.ArrUpd (subst s arr) (subst s index) (subst s expr) loc

instance Substitutable (Subs Type) Typed.TypedChain where
  subst s (Typed.Pure expr) = Typed.Pure $ subst s expr
  subst s (Typed.More ch op ty expr) = Typed.More (subst s ch) op (subst s ty) (subst s expr)