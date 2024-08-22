

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Server.Hover
  ( collectHoverInfo
  ) where


import qualified Language.LSP.Types as J
import           Pretty
import           Data.Loc           ( Located
                                    , locOf
                                    )
import           Data.Loc.Range
import           Server.IntervalMap ( IntervalMap )
import qualified Server.IntervalMap as IntervalMap
import           Syntax.Abstract                as UnTyped
import           Syntax.Typed                   as Typed

collectHoverInfo :: Typed.Program -> IntervalMap J.Hover
collectHoverInfo = collect

instance Pretty J.Hover where
  pretty = pretty . show

--------------------------------------------------------------------------------
-- helper function for annotating some syntax node with its type or kind

annotateType :: Located a => a -> Type -> IntervalMap J.Hover
annotateType node t = case fromLoc (locOf node) of
  Nothing    -> mempty
  Just range -> IntervalMap.singleton range hover
  where
    hover   = J.Hover content Nothing
    content = J.HoverContents $ J.markedUpContent "gcl" (toText t)

annotateKind :: Located a => a -> Kind -> IntervalMap J.Hover
annotateKind node k = case fromLoc (locOf node) of
  Nothing    -> mempty
  Just range -> IntervalMap.singleton range hover
  where
    hover   = J.Hover content Nothing
    content = J.HoverContents $ J.markedUpContent "gcl" (toText k)

--------------------------------------------------------------------------------

class Collect a where
  collect :: a -> IntervalMap J.Hover

{-
instance Collect a => Collect [a] where
  collect = undefined
-}

--------------------------------------------------------------------------------
-- Program

instance Collect Typed.Program where
  collect (Typed.Program defns decls exprs stmts _) = foldMap collect defns <> foldMap collect decls <> foldMap collect exprs <> foldMap collect stmts

--------------------------------------------------------------------------------
-- Definition

unkind :: KindedType -> Type
unkind (Typed.TBase base _ loc) = UnTyped.TBase base loc
unkind (Typed.TArray int ty loc) = UnTyped.TArray int (unkind ty) loc
unkind (Typed.TTuple int _) = UnTyped.TTuple int
unkind (Typed.TFunc ty1 ty2 loc) = UnTyped.TFunc (unkind ty1) (unkind ty2) loc
unkind (Typed.TOp op _) = UnTyped.TOp op
unkind (Typed.TData name _ loc) = UnTyped.TData name loc
unkind (Typed.TApp ty1 ty2 loc) = UnTyped.TApp (unkind ty1) (unkind ty2) loc
unkind (Typed.TVar name _ loc) = UnTyped.TVar name loc
unkind (Typed.TMetaVar name _ loc) = UnTyped.TMetaVar name loc

instance Collect Typed.Definition where
  collect (Typed.TypeDefn _ _ ctors _) = foldMap collect ctors
  collect (Typed.FuncDefnSig name kinded prop _) = annotateType name (unkind kinded) <> collect kinded <> maybe mempty collect prop
  collect (Typed.FuncDefn _name expr) = collect expr

instance Collect Typed.TypeDefnCtor where
  collect (Typed.TypeDefnCtor _name _tys) = mempty

instance Collect Typed.KindedType where
  collect (Typed.TBase base kind loc) = annotateKind loc kind
  collect (Typed.TArray int kinded loc) = collect kinded
  collect (Typed.TTuple i k) = mempty
  collect (Typed.TFunc l r _) = collect l <> collect r
  collect (Typed.TOp op kind) = annotateKind op kind
  collect (Typed.TData name kind _) = annotateKind name kind
  collect (Typed.TApp ty1 ty2 _) = collect ty1 <> collect ty2
  collect (Typed.TVar name kind _) = annotateKind name kind
  collect (Typed.TMetaVar name kind _) = annotateKind name kind

--------------------------------------------------------------------------------
-- Declaration

instance Collect Typed.Declaration where
  collect (Typed.ConstDecl names ty expr _) = annotateType names ty <> maybe mempty collect expr
  collect (Typed.VarDecl names ty expr _) = annotateType names ty <> maybe mempty collect expr

--------------------------------------------------------------------------------
-- Stmt

instance Collect Typed.Stmt where -- TODO: Display hover info for names.
  collect (Typed.Skip _) = mempty
  collect (Typed.Abort _) = mempty
  collect (Typed.Assign _names exprs _) = foldMap collect exprs
  collect (Typed.AAssign arr index rhs _) = collect arr <> collect index <> collect rhs
  collect (Typed.Assert expr _) = collect expr
  collect (Typed.LoopInvariant inv bnd _) = collect inv <> collect bnd
  collect (Typed.Do gdCmds _) = foldMap collect gdCmds
  collect (Typed.If gdCmds _) = foldMap collect gdCmds
  collect Typed.Spec {} = mempty
  collect Typed.Proof {} = mempty
  collect (Typed.Alloc _name exprs _) = foldMap collect exprs
  collect (Typed.HLookup _name expr _) = collect expr
  collect (Typed.HMutate e1 e2 _) = collect e1 <> collect e2
  collect (Typed.Dispose expr _) = collect expr
  collect (Typed.Block program _) = collect program

instance Collect Typed.GdCmd where
  collect (Typed.GdCmd gd stmts _) = collect gd <> foldMap collect stmts

--------------------------------------------------------------------------------

instance Collect Typed.Expr where
  collect (Typed.Lit _lit ty loc) = annotateType loc ty
  collect (Typed.Var name ty _) = annotateType name ty
  collect (Typed.Const name ty _) = annotateType name ty
  collect (Typed.Op op ty) = annotateType op ty
  collect (Typed.Chain ch) = collect ch
  collect (Typed.App expr1 expr2 _) = collect expr1 <> collect expr2
  collect (Typed.Lam name ty expr _) = annotateType name ty <> collect expr
  collect (Typed.Quant quantifier _bound restriction inner _) = collect quantifier <> collect restriction <> collect inner
  collect (Typed.ArrIdx expr1 expr2 _) = collect expr1 <> collect expr2
  collect (Typed.ArrUpd arr index expr _) = collect arr <> collect index <> collect expr
  collect (Typed.Case expr clauses _) = collect expr <> foldMap collect clauses
  collect (Typed.Subst orig pairs) = collect orig <> foldMap (\(_, expr) -> collect expr) pairs -- TODO: Not sure if this is correct.

instance Collect Typed.CaseClause where
  collect (Typed.CaseClause _pat expr) = collect expr

instance Collect Typed.Chain where
  collect (Typed.Pure expr) = collect expr
  collect (Typed.More chain op ty expr) = collect chain <> annotateType op ty <> collect expr

{-

instance Collect QuantOp' where
  collect (Left  op  ) = collect op
  collect (Right expr) = collect expr

instance Collect FuncClause where
  collect (FuncClause patterns body) = collect patterns >> collect body

instance Collect Type (J.Hover, Type) Pattern where
  collect PattLit{} = return ()
  collect (PattBinder n) = collect n
  collect (PattWildcard rng) = do
    result <- TypeChecking.lookupHoleScopeTree rng <$> get
    forM_ result (annotateType rng)
  collect (PattConstructor _ patts) = collect patts -}

--------------------------------------------------------------------------------
-- | Types

{-
instance Collect Type (J.Hover, Type) Type where
  collect = \case
    TBase _ _    -> return ()
    TArray i x _ -> collect i >> collect x
    TTuple xs    -> mapM_ collect xs
    TFunc x y _  -> collect x >> collect y
    TCon  x _ _  -> collect x
    TVar _ _     -> return ()
    TMetaVar _   -> return ()

instance Collect Type (J.Hover, Type) Interval where
  collect (Interval x y _) = collect x >> collect y

instance Collect Type (J.Hover, Type) Endpoint where
  collect = \case
    Including x -> collect x
    Excluding x -> collect x -}
