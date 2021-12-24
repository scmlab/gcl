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
import           Data.Loc                       ( (<-->)
                                                , Loc(..)
                                                , Located(locOf)
                                                )
import           Data.Loc.Range
import qualified Data.Map                      as Map
import qualified Syntax.Abstract               as A
import qualified Syntax.Abstract.Operator      as A
import qualified Syntax.Abstract.Util          as A
import           Syntax.Abstract.Util
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


instance ToAbstract Name Name where
  toAbstract = return

--------------------------------------------------------------------------------
-- | Program

instance ToAbstract Program A.Program where
  toAbstract prog@(Program ds stmts') = do
    (originalDecls, defns) <- foldl (<>) ([], mempty) <$> toAbstract ds

    let funcDefnSigsAsConstDecl =
          foldMap A.funcDefnSigsToConstDecl (A.defnFuncSigs defns)
    let typeDefnCtorAsConstDecl =
          foldMap A.typeDefnCtorsToConstDecl (A.defnTypes defns)
    let decls =
          originalDecls
            <> funcDefnSigsAsConstDecl -- add type of functions into declarations
            <> typeDefnCtorAsConstDecl -- add type of constructors' into declarations

    let (globProps, assertions) = ConstExpr.pickGlobals decls
    let pre =
          [ A.Assert (A.conjunct assertions) NoLoc | not (null assertions) ]
    stmts <- toAbstract stmts'

    return $ A.Program defns decls globProps (pre ++ stmts) (locOf prog)

instance ToAbstract (Either Declaration DefinitionBlock) ([A.Declaration], A.Definitions) where
  toAbstract (Left d) = do
    decls <- toAbstract d
    return ([decls], mempty)
  toAbstract (Right defnBlock) = do
    defns <- toAbstract defnBlock
    return ([], defns)

--------------------------------------------------------------------------------
-- | Definition

instance ToAbstract DefinitionBlock A.Definitions where
  toAbstract (DefinitionBlock _ decls _) = mconcat <$> mapM toAbstract decls

instance ToAbstract Definition A.Definitions where
  toAbstract (TypeDefn tok name binders _ cons) = do
    constructor <- toAbstract cons
    let typeDefn = A.TypeDefn name binders constructor (tok <--> cons)
    return $ A.Definitions { A.defnTypes    = Map.singleton name typeDefn
                           , A.defnFuncSigs = mempty
                           , A.defnFuncs    = mempty
                           }

  toAbstract (FuncDefnSig decl prop) = do
    (names, typ) <- toAbstract decl
    prop'        <- toAbstract prop
    let sigs =
          map (\name -> A.FuncDefnSig name typ prop' (decl <--> prop)) names
    return $ A.Definitions { A.defnTypes    = mempty
                           , A.defnFuncSigs = Map.fromList (zip names sigs)
                           , A.defnFuncs    = mempty
                           }

  toAbstract (FuncDefn name args _ body) = do
    body' <- toAbstract body
    let clause = [wrapLam args body']

    -- -- merge multiple FuncDefnClause into one
    -- mergeFuncDefnClauses :: [FuncDefnClause] -> Map Name [Expr]
    -- mergeFuncDefnClauses = Map.fromListWith mergeFuncDefnsOfTheSameName
    --   . map (\(FuncDefnClause name args body _) -> (name, [wrapLam args body]))
    -- where
    --   mergeFuncDefnsOfTheSameName :: [Expr] -> [Expr] -> [Expr]
    --   mergeFuncDefnsOfTheSameName = (<>)


    return $ A.Definitions { A.defnTypes    = mempty
                           , A.defnFuncSigs = mempty
                           , A.defnFuncs    = Map.singleton name clause
                           }

instance ToAbstract TypeDefnCtor A.TypeDefnCtor where
  toAbstract (TypeDefnCtor c ts) = A.TypeDefnCtor c <$> toAbstract ts

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
    Do _ a _           -> A.Do <$> toAbstract a <*> pure (locOf stmt)
    If _ a _           -> A.If <$> toAbstract a <*> pure (locOf stmt)
    SpecQM l           -> throwError l
    Spec  l t       r  -> pure (A.Spec t (rangeOf l <> rangeOf r))
    Proof _ anchors _  -> A.Proof <$> toAbstract anchors <*> pure (locOf stmt)
    Alloc p _ _ _ es _ -> A.Alloc p <$> toAbstract es <*> pure (locOf stmt)
    HLookup x _ _ e    -> A.HLookup x <$> toAbstract e <*> pure (locOf stmt)
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
        A.TTuple as'     -> pure $ A.TTuple as'
        A.TFunc a' b' _  -> pure $ A.TFunc a' b' (locOf t)
        A.TCon  a' b' _  -> pure $ A.TCon a' b' (locOf t)
        A.TVar a' _      -> pure $ A.TVar a' (locOf t)
        A.TMetaVar a'    -> pure $ A.TMetaVar a'

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
    Case _ expr _ cases ->
      A.Case <$> toAbstract expr <*> toAbstract cases <*> pure (locOf x)

instance ToAbstract CaseClause A.CaseClause where
  toAbstract (CaseClause patt _ body) =
    A.CaseClause <$> toAbstract patt <*> toAbstract body

instance ToAbstract Pattern A.Pattern where
  toAbstract (PattLit x      ) = A.PattLit <$> toAbstract x
  toAbstract (PattParen _ x _) = toAbstract x
  toAbstract (PattBinder   x ) = return $ A.PattBinder x
  toAbstract (PattWildcard x ) = return $ A.PattWildcard (rangeOf x)
  toAbstract (PattConstructor ctor patterns) =
    A.PattConstructor ctor <$> toAbstract patterns

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
