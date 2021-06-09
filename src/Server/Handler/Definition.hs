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
import           Data.Maybe                     ( maybeToList )
import qualified Data.Text                     as Text
import           Error
import           Language.LSP.Types
import           Server.DSL
import           Server.Interpreter.RealWorld
import           Server.Stab
import           Server.Util
import           Syntax.Abstract

ignoreErrors :: Either [Error] [LocationLink] -> [LocationLink]
ignoreErrors (Left  _errors  ) = []
ignoreErrors (Right locations) = locations

handler :: Uri -> Position -> ([LocationLink] -> ServerM ()) -> ServerM ()
handler uri pos responder = do
  case uriToFilePath uri of
    Nothing       -> return ()
    Just filepath -> do
      interpret filepath (responder . ignoreErrors) $ do
        source                          <- getSource
        program@(Program _ _ defns _ _) <- parseProgram source
        runGotoM uri defns $ stabM pos program

type GotoM = ReaderT Env CmdM

data Env = Env
  { envUri   :: Uri
  , envDefns :: Defns
  }

runGotoM :: Uri -> Defns -> GotoM a -> CmdM a
runGotoM uri defns = flip runReaderT (Env uri defns)

instance StabM GotoM Program LocationLink where
  stabM pos (Program _ _ _ stmts _) = do
    _ <- ask
    fmap concat (mapM (stabM pos) stmts)

instance StabM GotoM Stmt LocationLink where
  stabM pos = \case
    Assert a _          -> stabM pos a
    LoopInvariant a b _ -> (<>) <$> stabM pos a <*> stabM pos b
    _                   -> return []

instance StabM GotoM Expr LocationLink where
  stabM pos = \case
    Const name callerLoc -> do
      uri   <- asks envUri
      defns <- asks envDefns

      if pos `stabbed'` name
        then case Map.lookup name defns of
          Nothing   -> return []
          Just expr -> do
            let getLink = do
                  callerRange <- fromLoc callerLoc
                  calleeRange <- fromLoc (locOf expr)
                  return $ LocationLink (Just $ toRange callerRange)
                                        uri
                                        (toRange calleeRange)
                                        (toRange calleeRange)
            return $ maybeToList getLink
        else return []


    Paren a         -> stabM pos a
    Chain a _ c _   -> (<>) <$> stabM pos a <*> stabM pos c
    App a b _       -> (<>) <$> stabM pos a <*> stabM pos b
    Lam _ b _       -> stabM pos b
    Quant _ _ c d _ -> (<>) <$> stabM pos c <*> stabM pos d
    _               -> return []

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
