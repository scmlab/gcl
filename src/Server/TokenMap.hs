{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Server.TokenMap
  ( Token(..)
  , TokenMap
  , lookupIntervalMap
  , collectTokenMap
  ) where

import           Control.Monad.RWS
import           Data.IntMap                    ( IntMap )
import qualified Data.IntMap                   as IntMap
import           Data.Loc                       ( Located
                                                , Pos
                                                , locOf
                                                , posCoff
                                                )
import           Data.Loc.Range
import           Data.Map                       ( Map )
import qualified Data.Map                      as Map
import qualified Data.Map.Merge.Lazy           as Map
import           Data.Text                      ( Text )
import qualified GCL.Type                      as TypeChecking
import qualified Language.LSP.Types            as J
import           Pretty                         ( toText )
import           Render
import qualified Server.SrcLoc                 as SrcLoc
-- import qualified Server.SrcLoc                 as SrcLoc
import           Syntax.Abstract
import           Syntax.Common

newtype TokenMap = TokenMap (IntMap (Int, Token)) deriving (Monoid, Semigroup)

lookupIntervalMap :: TokenMap -> Pos -> Maybe Token
lookupIntervalMap (TokenMap m) pos =
  let offset = posCoff pos
  in  case IntMap.lookupLE offset m of
        Nothing                 -> Nothing
        Just (_start, (end, x)) -> if offset <= end then Just x else Nothing

collectTokenMap :: Program -> TokenMap
collectTokenMap program = runM program (collect program)

--------------------------------------------------------------------------------

-- | Information we want to collect of a node of Abstract syntax  
data Token = Token
  { tokenHoverAndType :: Maybe (J.Hover, Type)
  , tokenLocationLink :: Maybe J.LocationLink
  }

data Target = Target
  { targetHoverAndType     :: Maybe (J.Hover, Type)
  , targetLocationLinkToBe :: Maybe (Range -> J.LocationLink)
  }

emptyTarget :: Target
emptyTarget = Target Nothing Nothing

instance Render Token where
  render (Token _ _) = "Token"

addTypeToTarget :: Type -> Target -> Target
addTypeToTarget t target = target { targetHoverAndType = Just (hover, t) }
 where
  hover   = J.Hover content Nothing
  content = J.HoverContents $ J.markedUpContent "gcl" (toText t)

addLocationLinkToTarget :: (Range -> J.LocationLink) -> Target -> Target
addLocationLinkToTarget l target = target { targetLocationLinkToBe = Just l }

addType :: Type -> M ()
addType t = case fromLoc (locOf t) of
  Nothing    -> return ()
  Just range -> tell $ TokenMap $ IntMap.singleton
    (posCoff (rangeStart range))
    ( posCoff (rangeEnd range)
    , Token { tokenHoverAndType = Just (hover, t), tokenLocationLink = Nothing }
    )
   where
    hover   = J.Hover content Nothing
    content = J.HoverContents $ J.markedUpContent "gcl" (toText t)

fromTarget :: Range -> Target -> Token
fromTarget range (Target hover linkToBe) = Token hover $ case linkToBe of
  Nothing -> Nothing
  Just f  -> Just (f range)

--------------------------------------------------------------------------------

-- | A mapping from names to something else
type Scope a = Map Text a

-- | Accumulates the result of `TokenMap` in writer 
--   Stores stack of scopes in reader 
type M = RWS [Scope Target] TokenMap ()


runM :: Program -> M a -> TokenMap
runM program f = let (_, _, w) = runRWS f (programToScopes program) () in w

-- | Extracts Scopes from a Program 
programToScopes :: Program -> [Scope Target]
programToScopes (Program defns@(Definitions typeDefns _funcDefnSigs funcDefns) decls _ _ _)
  = [topLevelScope]
 where
  topLevelScope :: Map Text Target
  topLevelScope = Map.merge
    (Map.traverseMissing (\_ t -> pure $ addTypeToTarget t emptyTarget))
    (Map.traverseMissing (\_ l -> pure $ addLocationLinkToTarget l emptyTarget))
    (Map.zipWithAMatched
      (\_ t l ->
        pure $ addTypeToTarget t $ addLocationLinkToTarget l emptyTarget
      )
    )
    types
    (Map.mapKeys nameToText locationLinks)


  types :: Map Text Type
  types =
    -- run type checking to get the types of definitions/declarations
    case TypeChecking.runTM (TypeChecking.defnsAndDeclsToEnv defns decls) of
      Left  _   -> Map.empty -- ignore type errors
      Right env -> Map.mapKeys nameToText (TypeChecking.envLocalDefns env)

  locationLinks :: Map Name (Range -> J.LocationLink)
  locationLinks =
    locationLinksFromFuncDefns
      <> locationLinksFromDecls
      <> locationLinksFromTypeDefns

  locationLinksFromDecls :: Map Name (Range -> J.LocationLink)
  locationLinksFromDecls =
    makeLocationLinks $ Map.fromList $ concatMap splitDecl decls

  locationLinksFromFuncDefns :: Map Name (Range -> J.LocationLink)
  locationLinksFromFuncDefns = makeLocationLinks funcDefns

  locationLinksFromTypeDefns :: Map Name (Range -> J.LocationLink)
  locationLinksFromTypeDefns = makeLocationLinks typeDefns

  -- split a parallel declaration into many simpler declarations
  splitDecl :: Declaration -> [(Name, Declaration)]
  splitDecl decl@(ConstDecl names _ _ _) = [ (name, decl) | name <- names ]
  splitDecl decl@(VarDecl   names _ _ _) = [ (name, decl) | name <- names ]

--  Helper function for converting 
--      a Map of "names" and "targets" 
--   to a Map of "names" and functions 
--        (which will become LocationLinks when supplied with the range of "origin")
--
--  For example: 
--
--    ╔═════ where the user clicks ════╗
--    ║                                ║
--    ║             double 3           ║
--    ║  origin ──▶ ~~~~~~             ║
--    ║                                ║
--    ╚════════════════════════════════╝
--
--    ╔═══════ where it leads to ══════╗
--    ║                                ║
--    ║             double x = x * 2   ║
--    ║    name ──▶ ~~~~~~             ║
--    ║  target ──▶ ~~~~~~~~~~~~~~~~   ║
--    ║                                ║
--    ╚════════════════════════════════╝

makeLocationLinks
  :: Located a => Map Name a -> Map Name (Range -> J.LocationLink)
makeLocationLinks = Map.mapMaybeWithKey $ \name target -> do
  targetRange          <- fromLoc (locOf target)
  targetSelectionRange <- fromLoc (locOf name)
  let toLocationLink originSelectionRange = J.LocationLink
        { -- Span of the origin of this link.
          -- Used as the underlined span for mouse interaction. Defaults to the word
          -- range at the mouse position.
          J._originSelectionRange = Just
                                      $ SrcLoc.toLSPRange originSelectionRange
          -- The target resource identifier of this link.
        , J._targetUri            = J.filePathToUri (rangeFile targetRange)
          -- The full target range of this link. If the target for example is a
          -- symbol then target range is the range enclosing this symbol not including
          -- leading/trailing whitespace but everything else like comments. This
          -- information is typically used to highlight the range in the editor.
        , J._targetRange          = SrcLoc.toLSPRange targetRange
          -- The range that should be selected and revealed when this link is being
          -- followed, e.g the name of a function. Must be contained by the the
          -- '_targetRange'
        , J._targetSelectionRange = SrcLoc.toLSPRange targetSelectionRange
        }
  return toLocationLink

-- | See if a name is in a series of scopes (from local to global)
-- | Return the first result (which should be the most local target)
lookupScopes :: Text -> M (Maybe Target)
lookupScopes name = asks lookupScopesPrim
 where
  lookupScopesPrim :: [Scope a] -> Maybe a
  lookupScopesPrim scopes = foldl findFirst Nothing scopes

  findFirst :: Maybe a -> Scope a -> Maybe a
  findFirst (Just found) _     = Just found
  findFirst Nothing      scope = Map.lookup name scope

localScope :: [Name] -> M a -> M a
localScope = pushScope . scopeFromLocalBinders
 where
  pushScope :: Scope Target -> M a -> M a
  pushScope scope = local (scope :)

  scopeFromLocalBinders :: [Name] -> Scope Target
  scopeFromLocalBinders names =
    Map.map (`addLocationLinkToTarget` emptyTarget)
      $ Map.mapKeys nameToText
      $ makeLocationLinks
      $ Map.fromList
      $ zip names names

--------------------------------------------------------------------------------

-- | Given a Abstract syntax node, returns a mapping of Range-Info
class Collect a where
  collect :: a -> M ()

instance Collect a => Collect (Maybe a) where
  collect Nothing  = return ()
  collect (Just x) = collect x

instance Collect a => Collect [a] where
  collect = mapM_ collect

instance Collect a => Collect (Map k a) where
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
      Nothing     -> return ()
      Just target -> case fromLoc (locOf name) of
        Nothing    -> return ()
        Just range -> tell $ TokenMap $ IntMap.singleton
          (posCoff (rangeStart range))
          (posCoff (rangeEnd range), fromTarget range target)

--------------------------------------------------------------------------------
-- Program

instance Collect Program where
  collect (Program defns decls _ stmts _) = do
    collect defns
    collect decls
    collect stmts

--------------------------------------------------------------------------------
-- Definition

instance Collect Definitions where
  collect defns = do
    -- collect (defnTypes defns)
    collect (defnFuncSigs defns)
    collect (defnFuncs defns)

instance Collect FuncDefnSig where
  collect (FuncDefnSig _name t prop _) = do
    collect t
    collect prop

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
    Lit   _ _           -> return ()
    Var   a _           -> collect a
    Const a _           -> collect a
    Op op               -> collect op
    App a b _           -> (<>) <$> collect a <*> collect b
    Lam _ b _           -> collect b
    Quant op args c d _ -> do
      collect op
      localScope args $ do
        collect c
        collect d
    -- RedexStem/Redex will only appear in proof obligations, not in code 
    RedexStem{}  -> return ()
    Redex _      -> return ()
    ArrIdx e i _ -> do
      collect e
      collect i
    ArrUpd e i f _ -> do
      collect e
      collect i
      collect f
    -- TODO: provide types for tokens in patterns 
    Case e patterns _ -> do
      collect e
      collect patterns

instance Collect CaseConstructor where
  collect (CaseConstructor ctor args body) = do
    collect ctor
    localScope args $ do
      collect body

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

--------------------------------------------------------------------------------
-- | Types 

instance Collect Type where
  collect = \case
    TBase _ _    -> return ()
    TArray i x _ -> collect i >> collect x
    TFunc  x y _ -> collect x >> collect y
    TCon   x _ _ -> collect x
    TVar _ _     -> return ()

instance Collect Interval where
  collect (Interval x y _) = collect x >> collect y

instance Collect Endpoint where
  collect = \case
    Including x -> collect x
    Excluding x -> collect x
