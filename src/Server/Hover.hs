

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
import qualified GCL.Type           as TypeChecking
import           Server.IntervalMap ( IntervalMap )
import qualified Server.IntervalMap as IntervalMap
import           Syntax.Abstract
import           Syntax.Common
import           Syntax.Typed                   as Typed

collectHoverInfo :: Typed.TypedProgram -> IntervalMap (J.Hover, Type)
collectHoverInfo = collect

instance Pretty J.Hover where
  pretty = pretty . show

--------------------------------------------------------------------------------
-- helper function for annotating some syntax node with its type

annotateType :: Located a => a -> Type -> IntervalMap (J.Hover, Type)
annotateType node t = case fromLoc (locOf node) of
  Nothing    -> mempty
  Just range -> IntervalMap.singleton range (hover, t)
 where
  hover   = J.Hover content Nothing
  content = J.HoverContents $ J.markedUpContent "gcl" (toText t)

--------------------------------------------------------------------------------

class Collect a where
  collect :: a -> IntervalMap (J.Hover, Type)

{-
instance Collect a => Collect [a] where
  collect = undefined
-}

--------------------------------------------------------------------------------
-- Program

instance Collect Typed.TypedProgram where
  collect (Typed.Program defns decls exprs stmts _) = foldMap collect defns <> foldMap collect decls <> foldMap collect exprs <> foldMap collect stmts

--------------------------------------------------------------------------------
-- Definition

instance Collect Typed.TypedDefinition where
  collect (Typed.TypeDefn _ _ ctors _) = foldMap collect ctors
  collect (Typed.FuncDefnSig arg t prop _) = annotateType arg t <> maybe mempty collect prop
  collect (Typed.FuncDefn name exprs) = foldMap collect exprs

instance Collect Typed.TypedTypeDefnCtor where
  collect (Typed.TypedTypeDefnCtor name tys) = mempty

--------------------------------------------------------------------------------
-- Declaration

instance Collect Typed.TypedDeclaration where
  collect (Typed.ConstDecl names ty expr _) = annotateType names ty <> maybe mempty collect expr
  collect (Typed.VarDecl names ty expr _) = annotateType names ty <> maybe mempty collect expr

--------------------------------------------------------------------------------
-- Stmt

instance Collect Typed.TypedStmt where
  collect (Typed.Skip _) = mempty
  collect (Typed.Abort _) = mempty
  collect (Typed.Assign names exprs _) = foldMap collect exprs -- TODO: Display hover info for names.
  collect (Typed.AAssign arr index rhs _) = collect arr <> collect index <> collect rhs
  collect (Typed.Assert expr _) = collect expr
  collect (Typed.LoopInvariant inv bnd _) = collect inv <> collect bnd
  collect (Typed.Do gdCmds _) = foldMap collect gdCmds
  collect (Typed.If gdCmds _) = foldMap collect gdCmds
  collect Typed.Spec {} = mempty
  collect Typed.Proof {} = mempty

instance Collect Typed.TypedGdCmd where
  collect (Typed.TypedGdCmd gd stmts _) = collect gd <> foldMap collect stmts

--------------------------------------------------------------------------------

instance Collect Typed.TypedExpr where
  collect (Typed.Lit _lit ty loc) = annotateType loc ty
  collect (Typed.Var name ty _) = annotateType name ty
  collect (Typed.Const name ty _) = annotateType name ty
  collect (Typed.Op op ty) = annotateType op ty
  collect (Typed.App expr1 expr2 _) = collect expr1 <> collect expr2
  collect (Typed.Lam name ty expr _) = annotateType name ty <> collect expr
  collect (Typed.Quant quantifier _bound restriction inner _) = collect quantifier <> collect restriction <> collect inner
  collect (Typed.ArrIdx expr1 expr2 _) = collect expr1 <> collect expr2
  collect (Typed.ArrUpd arr index expr _) = collect arr <> collect index <> collect expr

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