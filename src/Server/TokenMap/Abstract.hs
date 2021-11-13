{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE FlexibleInstances #-}

module Server.TokenMap.Abstract
  ( Info(..)
  , IntervalMap
  , lookupIntervalMap
  , collectInfo
  ) where

import           Control.Monad.Reader
import           Control.Monad.Writer
import           Data.IntMap                    ( IntMap )
import qualified Data.IntMap                   as IntMap
import           Data.Loc                       ( Pos
                                                , locOf
                                                , posCoff
                                                )
import           Data.Loc.Range
import           Data.Map                       ( Map )
import qualified Data.Map                      as Map
import           Data.Text                      ( Text )
import qualified GCL.Type                      as TypeChecking
import qualified Language.LSP.Types            as J
import           Pretty                         ( toText )
import           Render
import           Server.Stab                    ( Scope )
import           Syntax.Abstract
import           Syntax.Common

type IntervalMap a = IntMap (Int, a)

lookupIntervalMap :: IntervalMap a -> Pos -> Maybe a
lookupIntervalMap m pos =
  let offset = posCoff pos
  in  case IntMap.lookupLE offset m of
        Nothing                 -> Nothing
        Just (_start, (end, x)) -> if offset <= end then Just x else Nothing

collectInfo :: Program -> IntervalMap Info
collectInfo program = runM program (collect program)

--------------------------------------------------------------------------------

-- | Information we want to collect of a node of Abstract syntax  
data Info = Info
  { infoHover :: J.Hover
  , infoType  :: Type
  }
  deriving (Eq, Show)

instance Render Info where
  render (Info _ t) = "Info " <+> render t

fromType :: Type -> Info
fromType t = Info { infoHover = hover, infoType = t }
 where
  hover   = J.Hover content Nothing
  content = J.HoverContents $ J.markedUpContent "gcl" (toText t)

addType :: Type -> M ()
addType t = case fromLoc (locOf t) of
  Nothing    -> return ()
  Just range -> tell $ IntMap.singleton
    (posCoff (rangeStart range))
    (posCoff (rangeEnd range), fromType t)

--------------------------------------------------------------------------------

type M = WriterT (IntervalMap Info) (Reader [Scope Info])

runM :: Program -> M a -> IntervalMap Info
runM (Program defns decls _ _ _) f = runReader (execWriterT f) [declScope]
 where
  declScope :: Map Text Info
  declScope =
    case TypeChecking.runTM (TypeChecking.defnsAndDeclsToEnv defns decls) of
    -- ignore type errors
      Left  _   -> Map.empty
      Right env -> Map.mapKeys nameToText
        $ Map.map fromType (TypeChecking.envLocalDefns env)

lookupScopes :: Text -> M (Maybe Info)
lookupScopes name = asks lookupScopesPrim
 where
  -- | See if a name is in a series of scopes (from local to global)
  -- | Return the first result (which should be the most local target)
  lookupScopesPrim :: [Scope a] -> Maybe a
  lookupScopesPrim scopes = foldl findFirst Nothing scopes

  findFirst :: Maybe a -> Scope a -> Maybe a
  findFirst (Just found) _     = Just found
  findFirst Nothing      scope = Map.lookup name scope

_pushScope :: Scope Info -> M a -> M a
_pushScope scope = local (scope :)

--------------------------------------------------------------------------------

-- | Given a Abstract syntax node, returns a mapping of Range-Info
class Collect a where
  collect :: a -> M ()

instance Collect a => Collect (Maybe a) where
  collect Nothing  = return ()
  collect (Just x) = collect x

instance Collect a => Collect [a] where
  collect = mapM_ collect

instance (Collect a, Collect b) => Collect (Either a b) where
  collect (Left  a) = collect a
  collect (Right a) = collect a

--------------------------------------------------------------------------------
-- Names

instance Collect Name where
  collect name = do
    result <- lookupScopes (nameToText name)
    case result of
      Nothing    -> return ()
      Just token -> case fromLoc (locOf name) of
        Nothing    -> return ()
        Just range -> tell $ IntMap.singleton
          (posCoff (rangeStart range))
          (posCoff (rangeEnd range), token)

--------------------------------------------------------------------------------
-- Program

instance Collect Program where
  collect (Program _ decls _ stmts _) = do
    collect decls
    collect stmts

--------------------------------------------------------------------------------
-- Declaration

instance Collect Declaration where
  collect = \case
    ConstDecl a _ c _ -> do
      collect a
      collect c
    VarDecl a _ c _ -> do
      collect a
      collect c

--------------------------------------------------------------------------------
-- Stmt

instance Collect Stmt where
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

instance Collect GdCmd where
  collect (GdCmd gd stmts _) = do
    collect gd
    collect stmts

--------------------------------------------------------------------------------

instance Collect Expr where
  collect = \case
    Lit _ _ -> return ()
    Var   a _        -> collect a
    Const a _        -> collect a
    Op op            -> collect op
    App a b _        -> (<>) <$> collect a <*> collect b
    Lam _ b _        -> collect b
    -- TODO: provide types for _args
    Quant op _args c d _ -> do
      collect op
      -- let argsScope = Map.fromList $ mapMaybe nameToLocationLink args
      collect c
      collect d
    -- RedexStem/Redex will only appear in proof obligations, not in code 
    RedexStem {} -> return ()
    Redex _ -> return ()
    ArrIdx e i _ -> do
      collect e
      collect i
    ArrUpd e i f _ -> do
      collect e
      collect i
      collect f
    -- TODO: provide types for tokens in patterns 
    Case e _ _ -> do
      collect e

instance Collect Op where
  collect (ChainOp op) = collect op
  collect (ArithOp op) = collect op

instance Collect ArithOp where
  collect op = addType (TypeChecking.arithOpTypes op)

instance Collect ChainOp where
  collect op = addType (TypeChecking.chainOpTypes op)

instance Collect QuantOp' where
  collect (Left  op  ) = collect op
  collect (Right expr) = collect expr
