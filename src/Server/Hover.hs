{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Server.Hover
  ( collectHoverInfo
  ) where

import           Control.Monad.RWS
import           Data.Loc                       ( Located
                                                , locOf
                                                )
import           Data.Loc.Range
import           GCL.Type                       (ScopeTreeZipper)
import qualified GCL.Type                      as TypeChecking
import qualified Language.LSP.Types            as J
import           Pretty                         ( Pretty(..)
                                                , toText
                                                )
import           Server.IntervalMap             (IntervalMap)
import qualified Server.IntervalMap             as IntervalMap
import           Syntax.Abstract
import           Syntax.Common

collectHoverInfo :: Program -> ScopeTreeZipper Type -> IntervalMap (J.Hover, Type)
collectHoverInfo program s = runM s (collect program)

instance Pretty J.Hover where
  pretty = pretty . show

--------------------------------------------------------------------------------
-- helper function for annotating some syntax node with its type

annotateType :: Located a => a -> Type -> M Type (J.Hover, Type) ()
annotateType node t = case fromLoc (locOf node) of
  Nothing    -> return ()
  Just range -> tell $ IntervalMap.singleton range (hover, t)
 where
  hover   = J.Hover content Nothing
  content = J.HoverContents $ J.markedUpContent "gcl" (toText t)

--------------------------------------------------------------------------------

-- | Extracts Scopes from a Program
--programToScopes :: Program -> [Scope Type]
--programToScopes prog = [topLevelScope]
 --where
  --topLevelScope :: Map Text Type
  --topLevelScope =
    -- run type checking to get the types of definitions/declarations
                  --case TypeChecking.runTypeCheck prog of
    --Left  _ -> Map.empty -- ignore type errors
    --Right _ -> mempty
    -- Map.mapMaybe
      --(\case
        --ScopeChecking.TypeDefnCtorInfo t  _ -> Just t
        --ScopeChecking.FuncDefnInfo     mt _ -> mt
        --ScopeChecking.VarTypeInfo      t  _ -> Just t
        --ScopeChecking.ConstTypeInfo    t  _ -> Just t
      --)
      --(snd env)

--------------------------------------------------------------------------------
-- Names
type M input output = RWS () (IntervalMap output) (ScopeTreeZipper input)

runM :: ScopeTreeZipper input -> M input output a -> IntervalMap output
runM s f = let (_, _, w) = runRWS f () s in w

class Collect input output a where
  collect :: a -> M input output ()

instance (Foldable t, Collect input output a) => Collect input output (t a) where
  collect = mapM_ collect

instance Collect Type (J.Hover, Type) Name where
  collect name = do
    result <- TypeChecking.lookupScopeTree name <$> get
    forM_ result (annotateType name)

--------------------------------------------------------------------------------
-- Program

instance Collect Type (J.Hover, Type) Program where
  collect (Program defns decls _ stmts _) = do
    collect defns
    collect decls
    collect stmts

--------------------------------------------------------------------------------
-- Definition
instance Collect Type (J.Hover, Type) Definition where
  collect (TypeDefn    _ _ ctors _) = collect ctors
  collect (FuncDefnSig _ t prop  _) = do
    collect t
    collect prop
  collect (FuncDefn n exprs) = do
    collect n
    collect exprs

instance Collect Type (J.Hover, Type) TypeDefnCtor where
  collect (TypeDefnCtor n ts) = do
    collect n
    collect ts
--------------------------------------------------------------------------------
-- Declaration

instance Collect Type (J.Hover, Type) Declaration where
  collect = \case
    ConstDecl a _ c _ -> do
      collect a
      collect c
    VarDecl a _ c _ -> do
      collect a
      collect c

--------------------------------------------------------------------------------
-- Stmt

instance Collect Type (J.Hover, Type) Stmt where
  collect = \case
    Assign a b _ -> do
      collect a
      collect b
    Assert a _          -> collect a
    LoopInvariant a b _ -> do
      collect a
      collect b
    Do a _ -> collect a
    If a _ -> collect a
    _      -> return ()

instance Collect Type (J.Hover, Type) GdCmd where
  collect (GdCmd gd stmts _) = do
    collect gd
    collect stmts

--------------------------------------------------------------------------------

instance Collect Type (J.Hover, Type) Expr where
  collect = \case
    Lit   _ _              -> return ()
    Var   a _              -> collect a
    Const a _              -> collect a
    Op op                  -> collect op
    App  a b _             -> collect a >> collect b
    Lam  _ b _             -> collect b
    Func a b _             -> collect a >> collect b
    Tuple as               -> mapM_ collect as
    Quant op _args _c _d _ -> do
      collect op
      -- TODO: push local scope of `args` onto the stack
      -- localScope args $ do
      --   collect c
      --   collect d
    -- RedexKernel/RedexShell will only appear in proof obligations, not in code
    RedexKernel{} -> return ()
    RedexShell{}  -> return ()
    ArrIdx e i _  -> do
      collect e
      collect i
    ArrUpd e i f _ -> do
      collect e
      collect i
      collect f
    -- TODO: provide types for tokens in patterns
    Case e _ _ -> do
      collect e
      -- collect patterns

-- instance Collect Type (J.Hover, Type) CaseClause where
--   collect (CaseClause ctor args body) = do
--     collect ctor
--     localScope args $ do
--       collect body

instance Collect Type (J.Hover, Type) Op where
  collect (ChainOp op) = collect op
  collect (ArithOp op) = collect op

instance Collect Type (J.Hover, Type) ArithOp where
  collect op = annotateType op (TypeChecking.arithOpTypes op)

instance Collect Type (J.Hover, Type) ChainOp where
  collect op = annotateType op (TypeChecking.chainOpTypes op)

instance Collect Type (J.Hover, Type) QuantOp' where
  collect (Left  op  ) = collect op
  collect (Right expr) = collect expr

instance Collect Type (J.Hover, Type) FuncClause where
  collect (FuncClause patterns body) = collect patterns >> collect body

-- TODO: implement this
instance Collect Type (J.Hover, Type) Pattern where
  collect PattLit{} = return ()
  collect (PattBinder n) = collect n
  collect (PattWildcard rng) = do
    result <- TypeChecking.lookupHoleScopeTree rng <$> get
    forM_ result (annotateType rng)
  collect (PattConstructor _ patts) = collect patts

--------------------------------------------------------------------------------
-- | Types

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
    Excluding x -> collect x
