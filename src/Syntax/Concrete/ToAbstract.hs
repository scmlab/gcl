{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
module Syntax.Concrete.ToAbstract where

import Control.Monad.Except ( Except, forM, throwError )
import Data.Loc (Loc (..), (<-->), Located (locOf))
import Syntax.Concrete
import Syntax.Concrete.Located()
import qualified Syntax.Abstract as A
import qualified Syntax.Abstract.Operator as A
import qualified Syntax.ConstExpr as ConstExpr
import Syntax.Common (Name)

--------------------------------------------------------------------------------

-- | Typeclass for converting Syntax.Concrete to Syntax.Abstract
class ToAbstract a b | a -> b where
  toAbstract :: a -> Except Loc b

--------------------------------------------------------------------------------

-- | Program / Declaration / Statement
instance ToAbstract Program A.Program where
  toAbstract (Program decls' stmts') = do
    decls <- concat <$> forM decls' toAbstract
    let letBindings = ConstExpr.pickLetBindings decls
    let (globProps, assertions) = ConstExpr.pickGlobals decls
    let pre = [A.Assert (A.conjunct assertions) NoLoc | not (null assertions)]
    stmts <- mapM toAbstract stmts'

    return $ A.Program decls globProps letBindings (pre ++ stmts) (decls' <--> stmts)

instance ToAbstract Declaration A.Declaration where
  toAbstract declaration = case declaration of
    ConstDecl _ decl -> do
      (name, body) <- toAbstract decl
      return $ A.ConstDecl name body Nothing (locOf decl)
    ConstDeclWithProp _ decl prop -> do
      (name, body) <- toAbstract decl
      prop' <- toAbstract prop
      return $ A.ConstDecl name body (Just prop') (locOf decl)
    VarDecl _ decl -> do
      (name, body) <- toAbstract decl
      return $ A.VarDecl name body Nothing (locOf decl)
    VarDeclWithProp _ decl prop -> do
      (name, body) <- toAbstract decl
      prop' <- toAbstract prop
      return $ A.VarDecl name body (Just prop') (locOf decl)
    LetDecl _ decl -> do
      (name, args, body) <- toAbstract decl
      return $ A.LetDecl name args body (locOf decl)

instance ToAbstract BlockDeclaration [A.Declaration] where
  toAbstract (BlockDeclaration _ decls _) = concat <$> mapM toAbstract decls

instance ToAbstract Stmt A.Stmt where
  toAbstract stmt = case stmt of
    Skip l -> pure (A.Skip l)
    Abort l -> pure (A.Abort l)
    Assign a _ b -> A.Assign (fromSepBy a) <$> mapM toAbstract (fromSepBy b) <*> pure (a <--> b)
    Assert l a r -> A.Assert <$> toAbstract a <*> pure (l <--> r)
    LoopInvariant l a _ _ _ b r -> A.LoopInvariant <$> toAbstract a <*> toAbstract b <*> pure (l <--> r)
    Do l a r -> A.Do <$> mapM toAbstract (fromSepBy a) <*> pure (l <--> r)
    If l a r -> A.If <$> mapM toAbstract (fromSepBy a) <*> pure (l <--> r)
    SpecQM l -> throwError l
    Spec l t r -> pure (A.Spec t (l <--> r))
    Proof l r -> pure (A.Proof (l <--> r))

instance ToAbstract GdCmd A.GdCmd where
  toAbstract (GdCmd a _ b) = A.GdCmd <$> toAbstract a <*> mapM toAbstract b <*> pure (a <--> b)

--------------------------------------------------------------------------------

-- Low level Declaration wrapper, and synonym types
instance ToAbstract Decl ([Name], A.Type) where
  toAbstract (Decl a _ b) = do
    b' <- toAbstract b
    return (fromSepBy a, b')

instance ToAbstract DeclProp A.Expr where
  toAbstract (DeclProp _ e _) = toAbstract e

instance ToAbstract DeclProp' A.Expr where
  toAbstract (Left prop) = toAbstract prop
  toAbstract (Right prop) = toAbstract prop

instance ToAbstract DeclBody (Name, [Name], A.Expr) where
  toAbstract (DeclBody n args _ b) = do
    b' <- toAbstract b
    return (n, args, b')

-- One BlockDecl can be parse into a ConstDecl or a ConstDecl and a LetDecl
instance ToAbstract BlockDecl [A.Declaration] where
  toAbstract declaration@(BlockDecl decl mDeclProp mDeclBody) = do
    (names, type') <- toAbstract decl
    case (mDeclProp, mDeclBody) of
      (Nothing, Nothing) -> do
        return [A.ConstDecl names type' Nothing (locOf declaration)]
      (Nothing, Just declBody) -> do
        (declBodyName, declBodyArgs, declBody') <- toAbstract declBody
        return [A.ConstDecl names type' Nothing (locOf declaration), A.LetDecl declBodyName declBodyArgs declBody' (locOf declBody)]
      (Just declProp, Nothing) -> do
        prop <- toAbstract declProp
        return [A.ConstDecl names type' (Just prop) (locOf declaration)]
      (Just declProp, Just declBody) -> do
        prop <- toAbstract declProp
        (declBodyName, declBodyArgs, declBody') <- toAbstract declBody
        return [A.ConstDecl names type' (Just prop) (locOf declaration), A.LetDecl declBodyName declBodyArgs declBody' (locOf declBody)]

instance ToAbstract Declaration' [A.Declaration] where
  toAbstract (Left d) = (: []) <$> toAbstract d
  toAbstract (Right bd) = toAbstract bd

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
  toAbstract (Interval a _ b) = A.Interval <$> toAbstract a <*> toAbstract b <*> pure (a <--> b)

-- | Base Type
instance ToAbstract TBase A.TBase where
  toAbstract (TInt _) = pure A.TInt
  toAbstract (TBool _) = pure A.TBool
  toAbstract (TChar _) = pure A.TChar

-- | Type
instance ToAbstract Type A.Type where
  toAbstract (TParen _ a _) = toAbstract a
  toAbstract (TBase a) = A.TBase <$> toAbstract a <*> pure (locOf a)
  toAbstract (TArray l a _ b) = A.TArray <$> toAbstract a <*> toAbstract b <*> pure (l <--> b)
  toAbstract (TFunc a _ b) = A.TFunc <$> toAbstract a <*> toAbstract b <*> pure (a <--> b)
  toAbstract (TVar a) = pure $ A.TVar a (locOf a)

--------------------------------------------------------------------------------

-- | Expressions
instance ToAbstract Expr A.Expr where
  toAbstract x = case x of
    Paren _ a _ -> A.Paren <$> toAbstract a
    Lit a -> A.Lit <$> toAbstract a <*> pure (locOf x)
    Var a -> pure $ A.Var a (locOf x)
    Const a -> pure $ A.Const a (locOf x)
    Op a -> pure $ A.Op a
    Chain a op b -> A.Chain <$> toAbstract a <*> pure op <*> toAbstract b <*> pure (locOf x)
    Arr arr _ i _ -> A.App <$> toAbstract arr <*> toAbstract i <*> pure (locOf x)
    App a b -> A.App <$> toAbstract a <*> toAbstract b <*> pure (locOf x)
    Quant _ a b _ c _ d _ -> A.Quant <$> toAbstractQOp a <*> pure b <*> toAbstract c <*> toAbstract d <*> pure (locOf x)
      where
        toAbstractQOp qop = case qop of
              Left op -> return . Left $ op
              Right expr -> do
                expr' <- toAbstract expr
                return . Right $ expr'

-- | Literals (Integer / Boolean / Character)
instance ToAbstract Lit A.Lit where
  toAbstract (LitInt a _) = pure $ A.Num a
  toAbstract (LitBool a _) = pure $ A.Bol a
  toAbstract (LitChar a _) = pure $ A.Chr a
--------------------------------------------------------------------------------

fromSepBy :: SepBy sep a -> [a]
fromSepBy (Head a) = [a]
fromSepBy (Delim a _ as) = a : fromSepBy as