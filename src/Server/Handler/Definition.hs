{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}

module Server.Handler.Definition
  ( handler
  ) where

import           Control.Monad.Reader
import           Data.Loc                       ( locOf, Loc )
import           Data.Loc.Range
import qualified Data.Map                      as Map
import           Data.Map                       ( Map )
import           Data.Maybe                     ( maybeToList )
import           Data.Text                      ( Text )
import           Error
import           Language.LSP.Types
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
        runGotoM uri program $ stabM pos program

type GotoM = ReaderT Env CmdM

data Env = Env
  { envUri   :: Uri
  , envDecls :: Map Text (Name, Declaration)
  }

runGotoM :: Uri -> Program -> GotoM a -> CmdM a
runGotoM uri (Program decls _ _ _ _) = flip runReaderT
                                                (Env uri decls')
 where
  decls' :: Map Text (Name, Declaration)
  decls' = Map.fromList (decls >>= map makeEntry . splitDecl)

  -- split a parallel declaration into many simpler declarations 
  splitDecl :: Declaration -> [(Name, Declaration)]
  splitDecl decl@(ConstDecl names _ _ _) = [ (name, decl) | name <- names ]
  splitDecl decl@(VarDecl   names _ _ _) = [ (name, decl) | name <- names ]
  splitDecl decl@(LetDecl   name  _ _ _) = [(name, decl)]

  makeEntry :: (Name, Declaration) -> (Text, (Name, Declaration))
  makeEntry (name, decl) = (nameToText name, (name, decl))

instance StabM GotoM Program LocationLink where
  stabM pos (Program decls _ _ stmts _) = do
    decls' <- concat <$> mapM (stabM pos) decls
    stmts' <- concat <$> mapM (stabM pos) stmts
    return (decls' <> stmts')

instance StabM GotoM Declaration LocationLink where
  stabM pos = \case
    ConstDecl _ _ c _ -> stabM pos c
    VarDecl _ _ c _ -> stabM pos c
    LetDecl _ _ c _ -> stabM pos c

instance StabM GotoM Stmt LocationLink where
  stabM pos = \case
    Assert a _          -> stabM pos a
    LoopInvariant a b _ -> (<>) <$> stabM pos a <*> stabM pos b
    _                   -> return []

instance StabM GotoM Expr LocationLink where
  stabM pos = \case
    Var name callerLoc -> stabDeclaration name callerLoc
    Const name callerLoc -> stabDeclaration name callerLoc
    Paren a         -> stabM pos a
    Chain a _ c _   -> (<>) <$> stabM pos a <*> stabM pos c
    App a b _       -> (<>) <$> stabM pos a <*> stabM pos b
    Lam _ b _       -> stabM pos b
    Quant _ _ c d _ -> (<>) <$> stabM pos c <*> stabM pos d
    _               -> return []
    where
      stabDeclaration :: Name -> Loc -> GotoM [LocationLink]
      stabDeclaration name callerLoc = do
        uri   <- asks envUri
        decls <- asks envDecls
        if pos `stabbed'` name
          then case Map.lookup (nameToText name) decls of
            Nothing                   -> return []
            Just (defnName, defnExpr) -> do
              let getLink = do
                    callerRange    <- fromLoc callerLoc
                    calleeRange    <- fromLoc (locOf defnExpr)
                    calleeSelRange <- fromLoc (locOf defnName)
                    return $ LocationLink (Just $ toRange callerRange)
                                          uri
                                          (toRange calleeRange)
                                          (toRange calleeSelRange)
              return $ maybeToList getLink
          else return []

-- instance StabM GotoM ProcDefn LocationLink where
--   stabM pos (ProcDefn _ _ process _) = stabM pos process

-- instance StabM GotoM Process LocationLink where
--   stabM pos = \case
--     Call name range -> do
--       uri <- asks envUri
--       defns <- asks envProcDefns
--       if pos `stabbed` name
--         then case Map.lookup (nameText name) defns of
--           Nothing -> return []
--           Just defnName -> do
--             let link =
--                   LocationLink
--                     (Just (toRange range))
--                     uri
--                     (toRange (rangeOf defnName))
--                     (toRange (rangeOf defnName))
--             return [link]
--         else return []
--     _ -> return []
