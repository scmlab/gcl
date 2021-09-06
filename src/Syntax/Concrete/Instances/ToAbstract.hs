{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TupleSections #-}
module Syntax.Concrete.Instances.ToAbstract where

import Control.Monad.Except ( Except, throwError )
import Data.Loc (Loc (..), (<-->), Located (locOf))
import Syntax.Concrete.Types
import Syntax.Concrete.Instances.Located()
import qualified Syntax.Abstract as A
import qualified Syntax.Abstract.Operator as A
import qualified Syntax.Abstract.Util as A
import qualified Syntax.ConstExpr as ConstExpr
import Syntax.Common (Name, Op(..))
import Data.Loc.Range
import Data.Either(partitionEithers)

--------------------------------------------------------------------------------

-- | Typeclass for converting Syntax.Concrete to Syntax.Abstract
class ToAbstract a b | a -> b where
  toAbstract :: a -> Except Range b

instance ToAbstract a b => ToAbstract (Maybe a) (Maybe b) where
  toAbstract Nothing = return Nothing
  toAbstract (Just x) = Just <$> toAbstract x

instance ToAbstract a b => ToAbstract [a] [b] where
  toAbstract = mapM toAbstract
--------------------------------------------------------------------------------

-- | Program / Declaration / Statement
instance ToAbstract Program A.Program where
  toAbstract prog@(Program ds stmts') = do
    (tdecls, ds', lds) <- foldl (<>) ([],[],[]) <$> toAbstract ds
    let decls = ds' <> foldMap A.extractQDCons tdecls -- add constructors' type into declarations
    let letBindings = ConstExpr.pickLetBindings lds
    let (globProps, assertions) = ConstExpr.pickGlobals decls
    let pre = [A.Assert (A.conjunct assertions) NoLoc | not (null assertions)]
    stmts <- toAbstract stmts'

    return $ A.Program tdecls decls globProps letBindings (pre ++ stmts) (locOf prog)

instance ToAbstract Declaration (Either A.TypeDeclaration A.Declaration) where
  toAbstract d = case d of
    ConstDecl _ decl -> do
        (name, body, prop) <- toAbstract decl
        return . Right $ A.ConstDecl name body prop (locOf d)
    VarDecl _ decl -> do
        (name, body, prop) <- toAbstract decl
        return . Right $ A.VarDecl name body prop (locOf d)
    TypeDecl _ tycon _ cons -> do
        Left <$> (A.TypeDecl <$> toAbstract tycon <*> toAbstract (fromSepBy cons) <*> pure (locOf d))


instance ToAbstract BlockDeclaration ([A.Declaration], [A.LetDeclaration]) where
  toAbstract (BlockDeclaration _ decls _) = partitionEithers <$> toAbstract decls

instance ToAbstract QTyCon A.QTyCon where
    toAbstract (QTyCon n ns) = pure $ A.QTyCon n ns

instance ToAbstract QDCon A.QDCon where
  toAbstract (QDCon c ts) = A.QDCon c <$> toAbstract ts

instance ToAbstract Stmt A.Stmt where
  toAbstract stmt = case stmt of
    Skip _ -> pure (A.Skip (locOf stmt))
    Abort _ -> pure (A.Abort (locOf stmt))
    Assign a _ b -> A.Assign (fromSepBy a) <$> toAbstract (fromSepBy b) <*> pure (locOf stmt)
    AAssign x _ i _ _ e -> A.AAssign (A.Var x (locOf x)) <$> toAbstract i <*> toAbstract e <*> pure (locOf stmt)
    Assert _ a _ -> A.Assert <$> toAbstract a <*> pure (locOf stmt)
    LoopInvariant _ a _ _ _ b _ -> A.LoopInvariant <$> toAbstract a <*> toAbstract b <*> pure (locOf stmt)
    Do _ a _ -> A.Do <$> toAbstract (fromSepBy a) <*> pure (locOf stmt)
    If _ a _ -> A.If <$> toAbstract (fromSepBy a) <*> pure (locOf stmt)
    SpecQM l -> throwError l
    Spec l t r -> pure (A.Spec t (rangeOf l <> rangeOf r))
    Proof _ anchors _ -> A.Proof <$> toAbstract anchors <*> pure (locOf stmt)
    Alloc p _ _ _ es _ -> A.Alloc p <$> toAbstract (fromSepBy es) <*> pure (locOf stmt)
    HLookup x _ _ e -> A.HLookup x <$> toAbstract e <*> pure (locOf stmt)
    HMutate _ e1 _ e2 -> A.HMutate <$> toAbstract e1 <*> toAbstract e2 <*> pure (locOf stmt)
    Dispose _ e -> A.Dispose <$> toAbstract e <*> pure (locOf stmt)

instance ToAbstract GdCmd A.GdCmd where
  toAbstract (GdCmd a _ b) = A.GdCmd <$> toAbstract a <*> toAbstract b <*> pure (a <--> b)

instance ToAbstract ProofAnchor A.ProofAnchor where
  toAbstract (ProofAnchor hash range) = pure $ A.ProofAnchor hash range

--------------------------------------------------------------------------------

-- Low level Declaration wrapper, and synonym types
instance ToAbstract DeclBase ([Name], A.Type) where
  toAbstract (DeclBase a _ b) = do
    b' <- toAbstract b
    return (fromSepBy a, b')

instance ToAbstract DeclProp A.Expr where
  toAbstract (DeclProp _ e _) = toAbstract e

instance ToAbstract DeclType ([Name], A.Type, Maybe A.Expr) where
  toAbstract (DeclType decl prop) = do
    (ns, t) <- toAbstract decl
    e <- toAbstract prop
    return (ns, t, e)

instance ToAbstract BlockDeclProp A.Expr where
  toAbstract (Left prop) = toAbstract prop
  toAbstract (Right prop) = toAbstract prop

instance ToAbstract DeclBody A.LetDeclaration where
  toAbstract d@(DeclBody n args _ b) = do
    A.LetDecl n args <$> toAbstract b <*> pure (locOf d)

instance ToAbstract BlockDeclType A.Declaration where
  toAbstract d@(BlockDeclType decl prop) = do
    (ns, t) <- toAbstract decl
    A.ConstDecl ns t <$> toAbstract prop <*> pure (locOf d)

-- One BlockDecl can be parse into a ConstDecl or a ConstDecl and a LetDecl
instance ToAbstract BlockDecl (Either A.Declaration A.LetDeclaration) where
  toAbstract (Left decl) = Left <$> toAbstract decl
  toAbstract (Right decl) = Right <$> toAbstract decl

instance ToAbstract Declaration' ([A.TypeDeclaration], [A.Declaration], [A.LetDeclaration]) where
  toAbstract (Left d) = uncurry (,, []) . partitionEithers . (:[]) <$> toAbstract d
  toAbstract (Right bd) = uncurry ([],,) <$> toAbstract bd

--------------------------------------------------------------------------------

-- | Endpoint
instance ToAbstract EndpointOpen A.Endpoint where
  toAbstract (IncludingOpening _ a) = A.Including <$> toAbstract a
  toAbstract (ExcludingOpening _ a) = A.Excluding <$> toAbstract a

instance ToAbstract EndpointClose A.Endpoint where
  toAbstract (IncludingClosing a _) = A.Including <$> toAbstract a
  toAbstract (ExcludingClosing a _) = A.Excluding <$> toAbstract a

-- | Interval
instance ToAbstract Interval A.Interval where
  toAbstract i@(Interval a _ b) = A.Interval <$> toAbstract a <*> toAbstract b <*> pure (locOf i)

-- | Base Type
instance ToAbstract TBase A.TBase where
  toAbstract (TInt _) = pure A.TInt
  toAbstract (TBool _) = pure A.TBool
  toAbstract (TChar _) = pure A.TChar

-- | Type
instance ToAbstract Type A.Type where
  toAbstract t =
    case t of
      (TBase a) -> A.TBase <$> toAbstract a <*> pure (locOf t)
      (TArray _ a _ b) -> A.TArray <$> toAbstract a <*> toAbstract b <*> pure (locOf t)
      (TFunc a _ b) -> A.TFunc <$> toAbstract a <*> toAbstract b <*> pure (locOf t)
      (TCon a) -> A.TCon <$> toAbstract a
      (TVar a) -> pure $ A.TVar a (locOf t)
      (TParen _ a _) -> do
        t' <- toAbstract a
        case t' of
          A.TBase a' _ -> pure$ A.TBase a' (locOf t)
          A.TArray a' b' _ -> pure $ A.TArray a' b' (locOf t)
          A.TFunc a' b' _ -> pure $ A.TFunc a' b' (locOf t)
          A.TCon a' -> pure $ A.TCon a'
          A.TVar a' _ -> pure $ A.TVar a' (locOf t)

--------------------------------------------------------------------------------

-- | Expressions
instance ToAbstract Expr A.Expr where
  toAbstract x = case x of
    Paren _ a _ -> toAbstract a
    Lit a -> A.Lit <$> toAbstract a <*> pure (locOf x)
    Var a -> pure $ A.Var a (locOf x)
    Const a -> pure $ A.Const a (locOf x)
    Op a -> pure $ A.Op (ArithOp a)
    Chain a op b -> A.Chain <$> toAbstract a <*> pure (ChainOp op) <*> toAbstract b <*> pure (locOf x)
    Arr arr _ i _ -> A.ArrIdx <$> toAbstract arr <*> toAbstract i <*> pure (locOf x)
    App a b -> A.App <$> toAbstract a <*> toAbstract b <*> pure (locOf x)
    Quant _ a b _ c _ d _ -> A.Quant <$> toAbstractQOp a <*> pure b <*> toAbstract c <*> toAbstract d <*> pure (locOf x)
      where
        toAbstractQOp qop = case qop of
              Left op -> return (A.Op op)
              Right expr -> toAbstract expr

-- | Literals (Integer / Boolean / Character)
instance ToAbstract Lit A.Lit where
  toAbstract (LitInt a _) = pure $ A.Num a
  toAbstract (LitBool a _) = pure $ A.Bol a
  toAbstract (LitChar a _) = pure $ A.Chr a
--------------------------------------------------------------------------------

fromSepBy :: SepBy sep a -> [a]
fromSepBy (Head a) = [a]
fromSepBy (Delim a _ as) = a : fromSepBy as
