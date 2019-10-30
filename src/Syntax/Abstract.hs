{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses, FunctionalDependencies #-}

module Syntax.Abstract where

import Control.Monad.State
import Control.Monad.Except
import Data.Text (Text)
import Data.Map (Map)
import Data.Loc (Loc)
import qualified Data.Map as Map

-- import Debug.Trace

import qualified Syntax.Concrete as C

type Index = Int

data Program = Program [Declaration] [Stmt]
  deriving (Show)

data Declaration
  = ConstDecl [Const] Type
  | VarDecl [Var] Type
  deriving (Show)

data Stmt
  = Skip
  | Abort
  | Assign  [Var] [Expr]
  | Assert  Pred
  | Do      Pred Expr [GdCmd]
  | If      (Maybe Pred) [GdCmd]
  | Spec    Pred Pred
  deriving (Show)

data GdCmd = GdCmd Pred [Stmt] deriving (Show)

unzipGdCmds :: [GdCmd] -> ([Pred], [[Stmt]])
unzipGdCmds = unzip . map (\(GdCmd x y) -> (x, y))

--------------------------------------------------------------------------------
-- | Affixing assertions to DO or IF constructs.

infixr 3 <:>
(<:>) :: Monad m => m a -> m [a] -> m [a]
(<:>) = liftM2 (:)

affixAssertions :: [C.Stmt] -> AbstractM [Stmt]
-- affixAssertions = undefined
affixAssertions      []  = return []
affixAssertions (  x:[]) = (:) <$> fromConcrete x <*> pure []
affixAssertions (x:y:xs) = case (x, y) of
  -- AssertWithBnd + DO : affix!
  (C.AssertWithBnd p e _, C.Do q _) ->
    Do  <$> fromConcrete p <*> fromConcrete e <*> mapM fromConcrete q
        <:> affixAssertions xs

  -- AssertWithBnd + _
  (C.AssertWithBnd _ _ loc, _) -> throwError $ ExcessBound loc

  -- Assert + DO
  (C.Assert _ loc, C.Do _ _) -> throwError $ MissingBound loc

  -- Assert + If : affix!
  (C.Assert p _, C.If q _) ->
    If  <$> fmap Just (fromConcrete p)
        <*> mapM fromConcrete q
        <:> affixAssertions xs

  -- _ + Do
  (_, C.Do _ loc) -> throwError $ MissingAssertion loc

  -- otherwise
  _  -> fromConcrete x <:> affixAssertions (y:xs)

--------------------------------------------------------------------------------
-- | Predicates

data BinRel = Eq | LEq | GEq | LTh | GTh
  deriving (Show, Eq)

data Pred = Term    BinRel Expr Expr
          | Implies Pred Pred
          | Conj    Pred Pred
          | Disj    Pred Pred
          | Neg     Pred
          | Lit     Bool
          | Hole    Index
          deriving (Show, Eq)

predEq :: Pred -> Pred -> Bool
predEq = (==)

substP :: Map Text Expr -> Pred -> Pred
substP env (Term rel e1 e2) = Term rel (substE env e1) (substE env e2)
substP env (Implies p q)    = Implies (substP env p) (substP env q)
substP env (Conj p q)       = Conj (substP env p) (substP env q)
substP env (Disj p q)       = Disj (substP env p) (substP env q)
substP env (Neg p)          = Neg (substP env p)
substP _   (Lit b)          = Lit b
substP _   (Hole _)         = undefined -- do we need it?

-- extractBnd :: Pred -> AbstractM (Pred, Expr)
-- extractBnd (Conj p (Bnd e)) = return (p,e)
-- extractBnd p = do {i <- index; return (p, HoleE i [])}

--------------------------------------------------------------------------------
-- | Expressions

data Lit  = Num Int
          | Bol Bool
          deriving (Show, Eq)

type OpName = Text
data Expr = VarE    Var
          | ConstE  Const
          | LitE    Lit
          | OpE     Expr   [Expr]
          | HoleE   Index  [Subst]
          deriving (Show, Eq)

type Subst = Map Text Expr

substE :: Subst -> Expr -> Expr
substE env (VarE x) =
  case Map.lookup x env of
    Just e -> e
    Nothing -> VarE x
substE env (ConstE x) =
  case Map.lookup x env of
    Just e -> e
    Nothing -> ConstE x
substE _   (LitE n)     = LitE n
substE env (OpE op es)  = OpE op (map (substE env) es)
substE env (HoleE idx subs) = HoleE idx (env:subs)

--------------------------------------------------------------------------------
-- | Variables and stuff

type Const = Text
type Var = Text
type Type = Text

--------------------------------------------------------------------------------
-- Converting from Concrete Syntax Tree

data SyntaxError = MissingAssertion Loc
                 | MissingBound     Loc
                 | ExcessBound      Loc
                 | Panic
                 deriving (Show)

type AbstractM = ExceptT SyntaxError (State Index)

abstract :: FromConcrete a b => a -> Either SyntaxError b
abstract = runAbstractM . fromConcrete

runAbstractM :: AbstractM a -> Either SyntaxError a
runAbstractM f = evalState (runExceptT f) 0

-- returns the current index and increment it in the state
index :: AbstractM Index
index = do
  i <- get
  put (succ i)
  return i


class FromConcrete a b | a -> b where
  fromConcrete :: a -> AbstractM b

instance FromConcrete C.Lit Lit where
  fromConcrete (C.Num x) = Num <$> pure x
  fromConcrete (C.Bol x) = Bol <$> pure x

instance FromConcrete C.Const Const where
  fromConcrete (C.Const x _) = pure x

instance FromConcrete C.Var Var where
  fromConcrete (C.Var x _) = pure x

instance FromConcrete C.Type Type where
  fromConcrete (C.Type x _) = pure x

instance FromConcrete C.Expr Expr where
  fromConcrete (C.VarE x    _) = VarE   <$> fromConcrete x
  fromConcrete (C.ConstE x  _) = ConstE <$> fromConcrete x
  fromConcrete (C.LitE x    _) = LitE   <$> fromConcrete x
  fromConcrete (C.OpE x xs  _) = OpE    <$> fromConcrete x <*> mapM fromConcrete xs
  fromConcrete (C.HoleE     _) = HoleE  <$> index <*> pure []

instance FromConcrete C.BinRel BinRel where
  fromConcrete (C.Eq  _) = pure Eq
  fromConcrete (C.LEq _) = pure LEq
  fromConcrete (C.GEq _) = pure GEq
  fromConcrete (C.LTh _) = pure LTh
  fromConcrete (C.GTh _) = pure GTh

instance FromConcrete C.Pred Pred where
  fromConcrete (C.Term p r q  _) = Term     <$> fromConcrete r
                                            <*> fromConcrete p
                                            <*> fromConcrete q
  fromConcrete (C.Implies p q _) = Implies  <$> fromConcrete p
                                            <*> fromConcrete q
  fromConcrete (C.Conj p q    _) = Conj     <$> fromConcrete p
                                            <*> fromConcrete q
  fromConcrete (C.Disj p q    _) = Disj     <$> fromConcrete p
                                            <*> fromConcrete q
  fromConcrete (C.Neg p       _) = Neg      <$> fromConcrete p
  fromConcrete (C.Lit p       _) = Lit      <$> pure p
  fromConcrete (C.Hole        _) = Hole     <$> index

instance FromConcrete C.Stmt Stmt where
  fromConcrete (C.Assert p   _) = Assert <$> fromConcrete p
  fromConcrete (C.Skip       _) = pure Skip
  fromConcrete (C.Abort      _) = pure Abort
  fromConcrete (C.Assign p q _) = Assign <$> mapM fromConcrete p
                                         <*> mapM fromConcrete q


  fromConcrete (C.If     p   _) = If     <$> pure Nothing
                                         <*> mapM fromConcrete p
  -- Panic because these cases should've been handled `affixAssertions`
  fromConcrete (C.AssertWithBnd _ _ _) = throwError $ Panic
  fromConcrete (C.Do     _ _) = throwError $ Panic

-- deals with missing Assertions and Bounds
instance FromConcrete [C.Stmt] [Stmt] where
  fromConcrete      []  = return []
  fromConcrete (x : []) = case x of
    C.Do _ loc -> throwError $ MissingAssertion loc
    _          -> fromConcrete x <:> pure []
  fromConcrete (x:y:xs) = affixAssertions (x:y:xs)

instance FromConcrete C.GdCmd GdCmd where
  fromConcrete (C.GdCmd p q _) = GdCmd  <$> fromConcrete p
                                        <*> fromConcrete q

instance FromConcrete C.Declaration Declaration where
  fromConcrete (C.ConstDecl p q _) = ConstDecl  <$> mapM fromConcrete p
                                                <*> fromConcrete q
  fromConcrete (C.VarDecl   p q _) = VarDecl    <$> mapM fromConcrete p
                                                <*> fromConcrete q

instance FromConcrete C.Program Program where
  fromConcrete (C.Program p q _) = Program  <$> mapM fromConcrete p
                                            <*> fromConcrete q
