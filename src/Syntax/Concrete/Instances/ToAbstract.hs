{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}

module Syntax.Concrete.Instances.ToAbstract where

import           Control.Monad.Except           ( Except
                                                , throwError
                                                )
import           Data.Loc                       ( (<-->)
                                                , Loc(..)
                                                , Located(locOf)
                                                )
import           Data.Loc.Range
import           Pretty.Util                    ( PrettyWithLoc(prettyWithLoc)
                                                , docToText
                                                , toDoc
                                                )
import qualified Syntax.Abstract               as A
import qualified Syntax.Abstract.Operator      as A
import           Syntax.Abstract.Util
import           Syntax.Common                  ( Name(..) )
import           Syntax.Concrete.Instances.Located
                                                ( )
import           Syntax.Concrete.Types
import qualified Syntax.ConstExpr              as ConstExpr
import GHC.Float (logDouble)

--------------------------------------------------------------------------------

-- | Typeclass for converting Syntax.Concrete to Syntax.Abstract
class ToAbstract a b | a -> b where
  toAbstract :: a -> Except Range b

instance ToAbstract a b => ToAbstract (Maybe a) (Maybe b) where
  toAbstract Nothing  = return Nothing
  toAbstract (Just x) = Just <$> toAbstract x

instance ToAbstract a b => ToAbstract [a] [b] where
  toAbstract = mapM toAbstract


instance ToAbstract Name Name where
  toAbstract = return

--------------------------------------------------------------------------------
-- | Program

instance ToAbstract Program A.Program where
  toAbstract prog@(Program ds stmts') = do
    (decls, defns) <- foldl (<>) ([], []) <$> toAbstract ds


    let (globProps, assertions) = ConstExpr.pickGlobals decls
    let pre =
          [ A.Assert (A.conjunct assertions) NoLoc | not (null assertions) ]
    stmts <- toAbstract stmts'

    return $ A.Program defns decls globProps (pre ++ stmts) (locOf prog)

instance ToAbstract (Either Declaration DefinitionBlock) ([A.Declaration], [A.Definition]) where
  toAbstract (Left d) = do
    decls <- toAbstract d
    return ([decls], [])
  toAbstract (Right defnBlock) = do
    defns <- toAbstract defnBlock
    return ([], defns)

--------------------------------------------------------------------------------
-- | Definition

instance ToAbstract DefinitionBlock [A.Definition] where
  toAbstract (DefinitionBlock _ defns _) = concat <$> toAbstract defns

instance ToAbstract Definition [A.Definition] where
  toAbstract (TypeDefn tok name binders _ cons) = do
    (: [])
      <$> (A.TypeDefn name binders <$> toAbstract cons <*> pure (tok <--> cons))
  toAbstract (FuncDefnSig decl prop) = do
    (names, typ) <- toAbstract decl
    mapM
      (\n -> A.FuncDefnSig n typ <$> toAbstract prop <*> pure (decl <--> prop))
      names
  toAbstract (FuncDefn name args _ body) = do
    body' <- toAbstract body
    return [A.FuncDefn name $ wrapLam args body']

instance ToAbstract TypeDefnCtor A.TypeDefnCtor where
  toAbstract (TypeDefnCtor c tys) = do
    tys' <- mapM toAbstract tys
    return $ A.TypeDefnCtor c tys'

--------------------------------------------------------------------------------
-- | Declaraion

instance ToAbstract Declaration A.Declaration where
  toAbstract d = case d of
    ConstDecl _ decl -> do
      (name, body, prop) <- toAbstract decl
      return $ A.ConstDecl name body prop (locOf d)
    VarDecl _ decl -> do
      (name, body, prop) <- toAbstract decl
      return $ A.VarDecl name body prop (locOf d)

--------------------------------------------------------------------------------
-- | Statement

instance ToAbstract Stmt A.Stmt where
  toAbstract stmt = case stmt of
    Skip  _      -> pure (A.Skip (locOf stmt))
    Abort _      -> pure (A.Abort (locOf stmt))
    Assign a _ b -> do
      A.Assign <$> toAbstract a <*> toAbstract b <*> pure (locOf stmt)
    AAssign x _ i _ _ e ->
      A.AAssign (A.Var x (locOf x)) <$> toAbstract i <*> toAbstract e <*> pure
        (locOf stmt)
    Assert _ a _ -> A.Assert <$> toAbstract a <*> pure (locOf stmt)
    LoopInvariant _ a _ _ _ b _ ->
      A.LoopInvariant <$> toAbstract a <*> toAbstract b <*> pure (locOf stmt)
    Do _ a _    -> A.Do <$> toAbstract a <*> pure (locOf stmt)
    If _ a _    -> A.If <$> toAbstract a <*> pure (locOf stmt)
    SpecQM l    -> throwError l
    Spec l xs r -> do
      let text = docToText $ toDoc $ prettyWithLoc (map (fmap show) xs)
      pure (A.Spec text (rangeOf l <> rangeOf r))
    Proof anchor contents _ r  -> pure $ A.Proof anchor contents r
    Alloc p _ _ _ es _ -> A.Alloc p <$> toAbstract es <*> pure (locOf stmt)
    HLookup x _ _ e    -> A.HLookup x <$> toAbstract e <*> pure (locOf stmt)
    HMutate _ e1 _ e2 ->
      A.HMutate <$> toAbstract e1 <*> toAbstract e2 <*> pure (locOf stmt)
    Dispose _ e -> A.Dispose <$> toAbstract e <*> pure (locOf stmt)
    Block _ p _ -> A.Block <$> toAbstract p <*> pure (locOf stmt)

instance ToAbstract GdCmd A.GdCmd where
  toAbstract (GdCmd a _ b) =
    A.GdCmd <$> toAbstract a <*> toAbstract b <*> pure (a <--> b)

-- instance ToAbstract ProofAnchor A.ProofAnchor where
--   toAbstract (ProofAnchor hash range) = pure $ A.ProofAnchor hash range

-- instance ToAbstract TextContents A.TextContents where
--   toAbstract (TextContents text range) = pure $ A.TextContents text range

--------------------------------------------------------------------------------

-- Low level Declaration wrapper, and synonym types
instance ToAbstract DeclBase ([Name], A.Type) where
  toAbstract (DeclBase a _ b) = (,) <$> toAbstract a <*> toAbstract b

instance ToAbstract DeclProp A.Expr where
  toAbstract (DeclProp _ e _) = toAbstract e

instance ToAbstract DeclType ([Name], A.Type, Maybe A.Expr) where
  toAbstract (DeclType decl prop) = do
    (ns, t) <- toAbstract decl
    e       <- toAbstract prop
    return (ns, t, e)

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
-- Base types were recognized as TCon (because Base types and TCon are identical at the syntactical level),
-- and to be converted to TBase here.
instance ToAbstract Type A.Type where
  toAbstract t = case t of
    (TBase a) -> A.TBase <$> toAbstract a <*> pure (locOf t)
    (TArray _ a _ b) ->
      A.TArray <$> toAbstract a <*> toAbstract b <*> pure (locOf t)
    (TOp op) -> pure $ A.TOp op
    (TData n _) -> pure $ A.TData n (locOf t)
    (TApp l r) -> A.TApp <$> toAbstract l <*> toAbstract r <*> pure (l <--> r)
    (TMetaVar a _) -> pure $ A.TMetaVar a (locOf t)
    (TParen _ a _) -> do
      t' <- toAbstract a
      case t' of
        A.TBase a' _     -> pure $ A.TBase a' (locOf t)
        A.TArray a' b' _ -> pure $ A.TArray a' b' (locOf t)
        A.TTuple as'     -> pure $ A.TTuple as'
        A.TFunc a' b' _  -> pure $ A.TFunc a' b' (locOf t)
        A.TOp op         -> pure $ A.TOp op
        A.TData name _   -> pure $ A.TData name (locOf t)
        A.TApp  a' b' _  -> pure $ A.TApp a' b' (locOf t)
        A.TVar a' _      -> pure $ A.TVar a' (locOf t)
        A.TMetaVar a' _  -> pure $ A.TMetaVar a' (locOf t)

--------------------------------------------------------------------------------

-- | Expressions
instance ToAbstract Expr A.Expr where
  toAbstract x = case x of
    Paren _ a _ -> toAbstract a
    Lit   a     -> A.Lit <$> toAbstract a <*> pure (locOf x)
    Var   a     -> pure $ A.Var a (locOf x)
    Const a     -> pure $ A.Const a (locOf x)
    Op    a     -> pure $ A.Op a
    Chain ch    -> A.Chain <$> toAbstract ch
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
        Right n@(Name _ l) -> return $ A.Const n l
    Case _ expr _ cases ->
      A.Case <$> toAbstract expr <*> toAbstract cases <*> pure (locOf x)

instance ToAbstract Chain A.Chain where
  toAbstract chain = case chain of
    Pure expr -> A.Pure <$> toAbstract expr <*> pure (locOf expr)
    More ch' op expr -> A.More <$> toAbstract ch' <*> pure op <*> toAbstract expr <*> pure (locOf expr)

instance ToAbstract CaseClause A.CaseClause where
  toAbstract (CaseClause patt _ body) =
    A.CaseClause <$> toAbstract patt <*> toAbstract body

instance ToAbstract Pattern A.Pattern where
  toAbstract (PattLit x      ) = A.PattLit <$> toAbstract x
  toAbstract (PattParen _ x _) = toAbstract x
  toAbstract (PattBinder   x ) = return $ A.PattBinder x
  toAbstract (PattWildcard x ) = return $ A.PattWildcard (rangeOf x)
  toAbstract (PattConstructor ctor pats) = do
    pats' <- mapM toAbstract pats
    return $ A.PattConstructor ctor pats'

-- | Literals (Integer / Boolean / Character)
instance ToAbstract Lit A.Lit where
  toAbstract (LitInt  a _) = pure $ A.Num a
  toAbstract (LitBool a _) = pure $ A.Bol a
  toAbstract (LitChar a _) = pure $ A.Chr a


--------------------------------------------------------------------------------

instance ToAbstract a b => ToAbstract (SepBy sep a) [b] where
  toAbstract (Head a) = do
    b <- toAbstract a
    return [b]
  toAbstract (Delim a _ as) = do
    b  <- toAbstract a
    bs <- toAbstract as
    return (b : bs)
