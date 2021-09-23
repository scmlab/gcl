{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE FlexibleContexts #-}

module Syntax.Concrete.Instances.ToAbstract where

import           Control.Monad.Except           ( Except
                                                , throwError
                                                )
import           Data.Either                    ( partitionEithers )
import           Data.Loc                       ( (<-->)
                                                , Loc(..)
                                                , Located(locOf)
                                                )
import           Data.Loc.Range
import qualified Syntax.Abstract               as A
import qualified Syntax.Abstract.Operator      as A
import qualified Syntax.Abstract.Util          as A
import           Syntax.Common                  ( Name )
import           Syntax.Concrete.Instances.Located
                                                ( )
import           Syntax.Concrete.Types
import qualified Syntax.ConstExpr              as ConstExpr

--------------------------------------------------------------------------------

-- | Typeclass for converting Syntax.Concrete to Syntax.Abstract
class ToAbstract a b | a -> b where
  toAbstract :: a -> Except Range b

instance ToAbstract a b => ToAbstract (Maybe a) (Maybe b) where
  toAbstract Nothing  = return Nothing
  toAbstract (Just x) = Just <$> toAbstract x

instance ToAbstract a b => ToAbstract [a] [b] where
  toAbstract = mapM toAbstract

--------------------------------------------------------------------------------
-- | Program

instance ToAbstract Program A.Program where
  toAbstract prog@(Program ds stmts') = do
    (originalDecls, typeDefns, funcDefnSigs, fundDefns) <- foldl (<>) ([], [], [], [])
      <$> toAbstract ds

    let funcDefnSigsAsConstDecl = foldMap A.funcDefnSigsToConstDecl funcDefnSigs 
    let typeDefnsAsConstDecl = foldMap A.typeDefnsToConstDecl typeDefns 
    let decls = originalDecls 
                    <> funcDefnSigsAsConstDecl -- add type of functions into declarations
                    <> typeDefnsAsConstDecl -- add type of constructors' into declarations
    let defns =
          A.Defns (A.collectTypeDefns typeDefns) (A.collectFuncDefns fundDefns)
    let (globProps, assertions) = ConstExpr.pickGlobals decls
    let pre =
          [ A.Assert (A.conjunct assertions) NoLoc | not (null assertions) ]
    stmts <- toAbstract stmts'

    return $ A.Program defns decls globProps (pre ++ stmts) (locOf prog)

instance ToAbstract (Either Declaration DefinitionBlock) ([A.Declaration], [A.TypeDefn], [A.FuncDefnTypeSig], [A.FuncDefn]) where
  toAbstract (Left d) = do
    d' <- toAbstract d
    case d' of
      Left  td -> return ([], [td], [], [])
      Right de -> return ([de], [], [], [])
  toAbstract (Right defnBlock) = do
    (sigs, funcs) <- toAbstract defnBlock
    return ([], [], sigs, funcs)

--------------------------------------------------------------------------------
-- | Definition 

instance ToAbstract DefinitionBlock ([A.FuncDefnTypeSig], [A.FuncDefn]) where
  toAbstract (DefinitionBlock _ decls _) =
    partitionEithers <$> toAbstract decls

instance ToAbstract Definition (Either A.FuncDefnTypeSig A.FuncDefn) where
  toAbstract (FuncDefnTypeSig decl prop) = do
    (ns, t) <- toAbstract decl
    Left
      <$> (A.FuncDefnTypeSig ns t <$> toAbstract prop <*> pure (decl <--> prop))
  toAbstract (FuncDefn decl) = Right <$> toAbstract decl

-- instance ToAbstract TypeDefn A.TypeDefn where
--   toAbstract (TypeDefn name binders _ ctors) = A.TypeDefnCtor c <$> toAbstract ts

instance ToAbstract TypeDefnCtor A.TypeDefnCtor where
  toAbstract (TypeDefnCtor c ts) = A.TypeDefnCtor c <$> toAbstract ts

--------------------------------------------------------------------------------
-- | Declaraion

instance ToAbstract Declaration (Either A.TypeDefn A.Declaration) where
  toAbstract d = case d of
    ConstDecl _ decl -> do
      (name, body, prop) <- toAbstract decl
      return . Right $ A.ConstDecl name body prop (locOf d)
    VarDecl _ decl -> do
      (name, body, prop) <- toAbstract decl
      return . Right $ A.VarDecl name body prop (locOf d)
    TYPEDEFN _ name binders _ cons -> do
      Left
        <$> (A.TypeDefn name binders <$> toAbstract (fromSepBy cons) <*> pure
              (locOf d)
            )

--------------------------------------------------------------------------------
-- | Statement 

instance ToAbstract Stmt A.Stmt where
  toAbstract stmt = case stmt of
    Skip  _ -> pure (A.Skip (locOf stmt))
    Abort _ -> pure (A.Abort (locOf stmt))
    Assign a _ b ->
      A.Assign (fromSepBy a) <$> toAbstract (fromSepBy b) <*> pure (locOf stmt)
    AAssign x _ i _ _ e ->
      A.AAssign (A.Var x (locOf x)) <$> toAbstract i <*> toAbstract e <*> pure
        (locOf stmt)
    Assert _ a _ -> A.Assert <$> toAbstract a <*> pure (locOf stmt)
    LoopInvariant _ a _ _ _ b _ ->
      A.LoopInvariant <$> toAbstract a <*> toAbstract b <*> pure (locOf stmt)
    Do _ a _          -> A.Do <$> toAbstract (fromSepBy a) <*> pure (locOf stmt)
    If _ a _          -> A.If <$> toAbstract (fromSepBy a) <*> pure (locOf stmt)
    SpecQM l          -> throwError l
    Spec  l t       r -> pure (A.Spec t (rangeOf l <> rangeOf r))
    Proof _ anchors _ -> A.Proof <$> toAbstract anchors <*> pure (locOf stmt)
    Alloc p _ _ _ es _ ->
      A.Alloc p <$> toAbstract (fromSepBy es) <*> pure (locOf stmt)
    HLookup x _ _ e -> A.HLookup x <$> toAbstract e <*> pure (locOf stmt)
    HMutate _ e1 _ e2 ->
      A.HMutate <$> toAbstract e1 <*> toAbstract e2 <*> pure (locOf stmt)
    Dispose _ e -> A.Dispose <$> toAbstract e <*> pure (locOf stmt)
    Block _ p _ -> A.Block <$> toAbstract p <*> pure (locOf stmt)

instance ToAbstract GdCmd A.GdCmd where
  toAbstract (GdCmd a _ b) =
    A.GdCmd <$> toAbstract a <*> toAbstract b <*> pure (a <--> b)

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
    e       <- toAbstract prop
    return (ns, t, e)

instance ToAbstract BlockDeclProp A.Expr where
  toAbstract (Left  prop) = toAbstract prop
  toAbstract (Right prop) = toAbstract prop

instance ToAbstract DeclBody A.FuncDefn where
  toAbstract d@(DeclBody n args _ b) = do
    A.FuncDefn n args <$> toAbstract b <*> pure (locOf d)

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
  toAbstract i@(Interval a _ b) =
    A.Interval <$> toAbstract a <*> toAbstract b <*> pure (locOf i)

-- | Base Type
instance ToAbstract TBase A.TBase where
  toAbstract (TInt  _) = pure A.TInt
  toAbstract (TBool _) = pure A.TBool
  toAbstract (TChar _) = pure A.TChar

-- | Type
instance ToAbstract Type A.Type where
  toAbstract t = case t of
    (TBase a) -> A.TBase <$> toAbstract a <*> pure (locOf t)
    (TArray _ a _ b) ->
      A.TArray <$> toAbstract a <*> toAbstract b <*> pure (locOf t)
    (TFunc a _ b) ->
      A.TFunc <$> toAbstract a <*> toAbstract b <*> pure (locOf t)
    (TCon a b    ) -> return $ A.TCon a b (a <--> b)
    (TVar a      ) -> pure $ A.TVar a (locOf t)
    (TParen _ a _) -> do
      t' <- toAbstract a
      case t' of
        A.TBase a' _     -> pure $ A.TBase a' (locOf t)
        A.TArray a' b' _ -> pure $ A.TArray a' b' (locOf t)
        A.TFunc  a' b' _ -> pure $ A.TFunc a' b' (locOf t)
        A.TCon   a' b' _ -> pure $ A.TCon a' b' (locOf t)
        A.TVar a' _      -> pure $ A.TVar a' (locOf t)

--------------------------------------------------------------------------------

-- | Expressions
instance ToAbstract Expr A.Expr where
  toAbstract x = case x of
    Paren _ a _ -> toAbstract a
    Lit   a     -> A.Lit <$> toAbstract a <*> pure (locOf x)
    Var   a     -> pure $ A.Var a (locOf x)
    Const a     -> pure $ A.Const a (locOf x)
    Op    a     -> pure $ A.Op a
    Arr arr _ i _ ->
      A.ArrIdx <$> toAbstract arr <*> toAbstract i <*> pure (locOf x)
    App a b -> A.App <$> toAbstract a <*> toAbstract b <*> pure (locOf x)
    Quant _ a b _ c _ d _ ->
      A.Quant
        <$> toAbstractQOp a
        <*> pure b
        <*> toAbstract c
        <*> toAbstract d
        <*> pure (locOf x)
     where
      toAbstractQOp qop = case qop of
        Left  op   -> return (A.Op op)
        Right expr -> toAbstract expr

-- | Literals (Integer / Boolean / Character)
instance ToAbstract Lit A.Lit where
  toAbstract (LitInt  a _) = pure $ A.Num a
  toAbstract (LitBool a _) = pure $ A.Bol a
  toAbstract (LitChar a _) = pure $ A.Chr a
--------------------------------------------------------------------------------

fromSepBy :: SepBy sep a -> [a]
fromSepBy (Head a      ) = [a]
fromSepBy (Delim a _ as) = a : fromSepBy as
