{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Server.Handler.Definition
  ( handler
  ) where

import           Control.Monad.Reader
import           Data.Loc                       ( locOf )
import           Data.Loc.Range
import qualified Data.Map                      as Map
import           Data.Map                       ( Map )
import           Data.Maybe                     ( mapMaybe )
import           Data.Text                      ( Text )
import           Error
import qualified Language.LSP.Types            as J
import           Language.LSP.Types      hiding ( Range )
import           Server.DSL
import           Server.Interpreter.RealWorld
import           Server.Stab
import           Server.Util
import           Syntax.Abstract
import           Syntax.Common                  ( Name
                                                , nameToText
                                                )

ignoreErrors :: Either [Error] [LocationLink] -> [LocationLink]
ignoreErrors (Left  _errors  ) = []
ignoreErrors (Right locations) = locations

handler :: Uri -> Position -> ([LocationLink] -> ServerM ()) -> ServerM ()
handler uri pos responder = do
  case uriToFilePath uri of
    Nothing       -> return ()
    Just filepath -> do
      interpret filepath (responder . ignoreErrors) $ do
        source  <- getSource
        program <- parseProgram source
        runGotoM program pos $ stabM program

--------------------------------------------------------------------------------

-- | A "Scope" is a mapping of names and LocationLinks
-- type Scope = Map Text (Range -> LocationLink)

-- -- | See if a name is in the scope
-- lookupScope :: Scope -> Name -> Maybe LocationLink
-- lookupScope scope name = case Map.lookup (nameToText name) scope of
--   Nothing             -> Nothing
--   Just toLocationLink -> do
--     case fromLoc (locOf name) of
--       Nothing          -> Nothing
--       Just callerRange -> Just (toLocationLink callerRange)

-- -- | See if a name is in a series of scopes (from local to global)
-- -- | Return the first result (which should be the most local target)
-- lookupScopes :: [Scope] -> Name -> Maybe LocationLink
-- lookupScopes scopes name = foldl findFirst Nothing scopes
--  where
--   findFirst :: Maybe LocationLink -> Scope -> Maybe LocationLink
--   findFirst (Just found) _     = Just found
--   findFirst Nothing      scope = lookupScope scope name

nameToLocationLink :: Name -> Maybe (Text, Range -> LocationLink)
nameToLocationLink arg = do
  targetRange <- fromLoc (locOf arg)
  let targetUri = J.filePathToUri (rangeFile targetRange)

  let text      = nameToText arg
  let toLocationLink callerRange = LocationLink (Just $ toRange callerRange)
                                                targetUri
                                                (toRange targetRange)
                                                (toRange targetRange)

  return (text, toLocationLink)
--------------------------------------------------------------------------------

type GotoM = ReaderT Position (ReaderT [Scope (Range -> LocationLink)] CmdM)

instance HasPosition GotoM where
  askPosition = ask

instance HasScopes GotoM (Range -> LocationLink) where
  askScopes = lift ask
  localScope scope p = do
    pos <- ask
    lift $ local (scope :) $ runReaderT p pos

runGotoM :: Program -> Position -> GotoM a -> CmdM a
runGotoM (Program _ decls _ _ _ _) pos f = runReaderT (runReaderT f pos)
                                                    [declScope]
 where
  declScope :: Map Text (Range -> LocationLink)
  declScope = Map.fromList (decls >>= mapMaybe declToLocationLink . splitDecl)

  -- split a parallel declaration into many simpler declarations
  splitDecl :: Declaration -> [(Name, Declaration)]
  splitDecl decl@(ConstDecl names _ _ _) = [ (name, decl) | name <- names ]
  splitDecl decl@(VarDecl names _ _ _) = [ (name, decl) | name <- names ]
  splitDecl decl@(LetDecl name _ _ _) = [(name, decl)]

  -- convert a declaration (and its name) to a LocationLink (that is waiting for the caller's Range)
  declToLocationLink
    :: (Name, Declaration) -> Maybe (Text, Range -> LocationLink)
  declToLocationLink (name, decl) = do
    targetRange    <- fromLoc (locOf decl)
    targetSelRange <- fromLoc (locOf name)
    let targetUri = J.filePathToUri (rangeFile targetRange)

    let text      = nameToText name
    let toLocationLink callerRange = LocationLink
          (Just $ toRange callerRange)
          targetUri
          (toRange targetRange)
          (toRange targetSelRange)

    return (text, toLocationLink)

instance StabM GotoM Program LocationLink where
  stabM (Program _ decls _ _ stmts _) =
    (<>) <$> stabLocated decls <*> stabLocated stmts

instance StabM GotoM Declaration LocationLink where
  stabM = \case
    ConstDecl _ _    c _ -> stabLocated c
    VarDecl   _ _    c _ -> stabLocated c
    LetDecl   _ args c _ -> do
      -- creates a local scope for arguments
      let argsScope = Map.fromList $ mapMaybe nameToLocationLink args
      -- temporarily prepend this local scope to the scope list

      localScope argsScope $ stabM c
    -- TODO : TypeDecl StabM

instance StabM GotoM Stmt LocationLink where
  stabM = \case
    Assign a b _        -> (<>) <$> stabLocated a <*> stabLocated b
    Assert a _          -> stabLocated a
    LoopInvariant a b _ -> (<>) <$> stabLocated a <*> stabLocated b
    Do a _              -> stabLocated a
    If a _              -> stabLocated a
    _                   -> return []

instance StabM GotoM GdCmd LocationLink where
  stabM (GdCmd gd stmts _) = (<>) <$> stabLocated gd <*> stabLocated stmts

instance StabM GotoM Expr LocationLink where
  stabM = \case
    Var   a _          -> stabLocated a
    Const a _          -> stabLocated a
    Chain a _ c _      -> (<>) <$> stabLocated a <*> stabLocated c
    App a b _          -> (<>) <$> stabLocated a <*> stabLocated b
    Lam _ b _          -> stabLocated b
    Quant _ args c d _ -> do
      -- creates a local scope for arguments
      let argsScope = Map.fromList $ mapMaybe nameToLocationLink args
      -- temporarily prepend this local scope to the scope list
      localScope argsScope $ (<>) <$> stabLocated c <*> stabLocated d
    _ -> return []

instance StabM GotoM Name LocationLink where
  stabM name = do
    result <- lookupScopes (nameToText name)
    case result of
      Nothing             -> return []
      Just toLocationLink -> case fromLoc (locOf name) of
        Nothing          -> return []
        Just callerRange -> return [toLocationLink callerRange]
