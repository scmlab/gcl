{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}

module Server.Handler.Definition
  ( handler
  ) where

import           Control.Monad.Reader
import           Data.Loc                       ( locOf )
import           Data.Loc.Range
import qualified Data.Map                      as Map
import           Data.Map                       ( Map )
import           Data.Maybe                     ( mapMaybe
                                                , maybeToList
                                                )
import           Data.Text                      ( Text )
import           Error
import       qualified    Language.LSP.Types as J 
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
        runGotoM program $ stabM pos program

--------------------------------------------------------------------------------

-- | A "Scope" is a mapping of names and LocationLinks
type Scope = Map Text (Range -> LocationLink)


-- | See if a name is in the scope
lookupScope :: Scope -> Name -> Maybe LocationLink
lookupScope scope name = case Map.lookup (nameToText name) scope of
  Nothing             -> Nothing
  Just toLocationLink -> do
    case fromLoc (locOf name) of
      Nothing          -> Nothing
      Just callerRange -> Just (toLocationLink callerRange)

-- | See if a name is in a series of scopes (from local to global)
-- | Return the first result (which should be the most local target)
lookupScopes :: [Scope] -> Name -> Maybe LocationLink
lookupScopes scopes name = foldl findFirst Nothing scopes
 where
  findFirst :: Maybe LocationLink -> Scope -> Maybe LocationLink
  findFirst (Just found) _     = Just found
  findFirst Nothing      scope = lookupScope scope name

nameToLocationLink
  :: Name -> Maybe (Text, Range -> LocationLink)
nameToLocationLink arg = do
  targetRange    <- fromLoc (locOf arg)
  let targetUri = J.filePathToUri (rangeFile targetRange)

  let text = nameToText arg
  let toLocationLink callerRange = LocationLink
        (Just $ toRange callerRange)
        targetUri
        (toRange targetRange)
        (toRange targetRange)

  return (text, toLocationLink)
--------------------------------------------------------------------------------

type GotoM = ReaderT [Scope] CmdM

runGotoM :: Program -> GotoM a -> CmdM a
runGotoM (Program decls _ _ _ _) = flip runReaderT [declScope]
 where
  declScope :: Map Text (Range -> LocationLink)
  declScope = Map.fromList (decls >>= mapMaybe declToLocationLink . splitDecl)

  -- split a parallel declaration into many simpler declarations 
  splitDecl :: Declaration -> [(Name, Declaration)]
  splitDecl decl@(ConstDecl names _ _ _) = [ (name, decl) | name <- names ]
  splitDecl decl@(VarDecl   names _ _ _) = [ (name, decl) | name <- names ]
  splitDecl decl@(LetDecl   name  _ _ _) = [(name, decl)]

  -- convert a declaration (and its name) to a LocationLink (that is waiting for the caller's Range)
  declToLocationLink
    :: (Name, Declaration) -> Maybe (Text, Range -> LocationLink)
  declToLocationLink (name, decl) = do
    targetRange    <- fromLoc (locOf decl)
    targetSelRange <- fromLoc (locOf name)
    let targetUri = J.filePathToUri (rangeFile targetRange)

    let text = nameToText name
    let toLocationLink callerRange = LocationLink
          (Just $ toRange callerRange)
          targetUri
          (toRange targetRange)
          (toRange targetSelRange)

    return (text, toLocationLink)


instance StabM GotoM Program LocationLink where
  stabM pos (Program decls _ _ stmts _) = do
    decls' <- concat <$> mapM (stabM pos) decls
    stmts' <- concat <$> mapM (stabM pos) stmts
    return (decls' <> stmts')

instance StabM GotoM Declaration LocationLink where
  stabM pos = \case
    ConstDecl _ _ c _ -> stabM pos c
    VarDecl   _ _ c _ -> stabM pos c
    LetDecl   _ args c _ -> do 
      -- creates a local scope for arguments 
      let argsScope = Map.fromList $ mapMaybe nameToLocationLink args
      -- temporarily preppend this local scope to the scope list 
      local (argsScope :) $ stabM pos c
instance StabM GotoM Stmt LocationLink where
  stabM pos = \case
    Assign a b _        -> (<>) <$> stabM pos a <*> stabM pos b
    Assert a _          -> stabM pos a
    LoopInvariant a b _ -> (<>) <$> stabM pos a <*> stabM pos b
    Do a _              -> stabM pos a
    If a _              -> stabM pos a
    _                   -> return []

instance StabM GotoM GdCmd LocationLink where
  stabM pos (GdCmd gd stmts _) = (<>) <$> stabM pos gd <*> stabM pos stmts

instance StabM GotoM Expr LocationLink where
  stabM pos = \case
    Var   a _       -> stabM pos a
    Const a _       -> stabM pos a
    Paren a         -> stabM pos a
    Chain a _ c _   -> (<>) <$> stabM pos a <*> stabM pos c
    App a b _       -> (<>) <$> stabM pos a <*> stabM pos b
    Lam _ b _       -> stabM pos b
    Quant _ args c d _ -> do 
      -- creates a local scope for arguments 
      let argsScope = Map.fromList $ mapMaybe nameToLocationLink args
      -- temporarily preppend this local scope to the scope list 
      local (argsScope :) $ (<>) <$> stabM pos c <*> stabM pos d
    _               -> return []

instance StabM GotoM Name LocationLink where
  stabM pos name = do
    if pos `stabbed'` name
      then do
        scopes <- ask
        return $ maybeToList $ lookupScopes scopes name
      else return []
