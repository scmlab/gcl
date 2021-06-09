{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}

module Server.Handler.Definition
  ( handler
  ) where

import           Control.Monad.Reader
import           Data.Loc                       ( locOf
                                                )
import           Data.Loc.Range
import qualified Data.Map                      as Map
import           Data.Map                       ( Map )
import           Data.Maybe                     ( mapMaybe
                                                )
import           Data.Text                      ( Text )
import           Error
import           Language.LSP.Types      hiding ( Range )
import           Server.DSL
import           Server.Interpreter.RealWorld
import           Server.Stab
import           Server.Util
import           Syntax.Abstract
import           Syntax.Common                  ( Name(Name)
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
  , envDecls :: Map Text (Range -> LocationLink)
  }

runGotoM :: Uri -> Program -> GotoM a -> CmdM a
runGotoM uri (Program decls _ _ _ _) = flip runReaderT (Env uri decls')
 where
  decls' :: Map Text (Range -> LocationLink)
  decls' = Map.fromList (decls >>= mapMaybe declToLocationLink . splitDecl)

  -- split a parallel declaration into many simpler declarations 
  splitDecl :: Declaration -> [(Name, Declaration)]
  splitDecl decl@(ConstDecl names _ _ _) = [ (name, decl) | name <- names ]
  splitDecl decl@(VarDecl   names _ _ _) = [ (name, decl) | name <- names ]
  splitDecl decl@(LetDecl   name  _ _ _) = [(name, decl)]

  declToLocationLink
    :: (Name, Declaration) -> Maybe (Text, Range -> LocationLink)
  declToLocationLink (name, decl) = do
    calleeRange    <- fromLoc (locOf decl)
    calleeSelRange <- fromLoc (locOf name)

    let text = nameToText name
    let toLocationLink callerRange = LocationLink
          (Just $ toRange callerRange)
          uri
          (toRange calleeRange)
          (toRange calleeSelRange)

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
    LetDecl   _ _ c _ -> stabM pos c

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
    Quant _ _ c d _ -> (<>) <$> stabM pos c <*> stabM pos d
    _               -> return []

instance StabM GotoM Name LocationLink where
  stabM pos name@(Name text callerLoc) = do
    decls <- asks envDecls
    if pos `stabbed'` name
      then case Map.lookup text decls of
        Nothing             -> return []
        Just toLocationLink -> do
          case fromLoc callerLoc of
            Nothing          -> return []
            Just callerRange -> return [toLocationLink callerRange]
      else return []
