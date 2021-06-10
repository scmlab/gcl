
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}


{-# LANGUAGE FlexibleInstances #-}
module Server.Handler.Hover
  ( handler
  ) where

import           Error                          ( Error )
import           Server.Interpreter.RealWorld
import           Server.Stab
import           Syntax.Abstract

import           Control.Monad.Except
import           Control.Monad.Reader
import           Data.Loc                       ( locOf )
import           Data.Map                       ( Map )
import qualified Data.Map                      as Map
import           Data.Maybe                     ( listToMaybe
                                                , maybeToList
                                                )
import           Data.Text                      ( Text )
import qualified GCL.Type                      as Type
import           Language.LSP.Types      hiding ( Range )
import           Pretty                         ( toText )
import           Server.DSL
import           Syntax.Common


ignoreErrors :: Either [Error] (Maybe Hover) -> Maybe Hover
ignoreErrors (Left  _errors  ) = Nothing
ignoreErrors (Right locations) = locations

handler :: Uri -> Position -> (Maybe Hover -> ServerM ()) -> ServerM ()
handler uri pos responder = case uriToFilePath uri of
  Nothing       -> return ()
  Just filepath -> do
    interpret filepath (responder . ignoreErrors) $ do
      source  <- getSource
      program <- parseProgram source
      hovers  <- runHoverM program $ stabM pos program
      return $ listToMaybe hovers

--------------------------------------------------------------------------------

-- | A "Scope" is a mapping of names and Hovers
type Scope = Map Text Hover

-- | See if a name is in the scope
lookupScope :: Scope -> Name -> Maybe Hover
lookupScope scope name = Map.lookup (nameToText name) scope

-- | See if a name is in a series of scopes (from local to global)
-- | Return the first result (which should be the most local target)
lookupScopes :: [Scope] -> Name -> Maybe Hover
lookupScopes scopes name = foldl findFirst Nothing scopes
 where
  findFirst :: Maybe Hover -> Scope -> Maybe Hover
  findFirst (Just found) _     = Just found
  findFirst Nothing      scope = lookupScope scope name


--------------------------------------------------------------------------------

type HoverM = ReaderT [Scope] CmdM

runHoverM :: Program -> HoverM a -> CmdM a
runHoverM (Program decls _ _ _ _) = flip runReaderT [declScope]
 where
  declScope :: Map Text Hover
  declScope = case runExcept (foldM Type.inferDecl Map.empty decls) of
    -- ignore type errors 
    Left  _   -> Map.empty
    Right env -> Map.mapKeys nameToText $ Map.map typeToHover env

typeToHover :: Type -> Hover
typeToHover t = Hover content Nothing
  where content = HoverContents $ markedUpContent "gcl" (toText t)

instance StabM HoverM Stmt Hover where
  stabM pos x = whenInRange' pos (locOf x) $ case x of
    Assign a b _        -> (<>) <$> stabM pos a <*> stabM pos b
    Assert a _          -> stabM pos a
    LoopInvariant a b _ -> (<>) <$> stabM pos a <*> stabM pos b
    Do a _              -> stabM pos a
    If a _              -> stabM pos a
    _                   -> return []

instance StabM HoverM GdCmd Hover where
  stabM pos (GdCmd gd stmts l) =
    whenInRange' pos l $ (<>) <$> stabM pos gd <*> stabM pos stmts

instance StabM HoverM Declaration Hover where
  stabM pos x = whenInRange' pos (locOf x) $ case x of
    ConstDecl _ _ c _ -> stabM pos c
    VarDecl   _ _ c _ -> stabM pos c
    LetDecl   _ _ c _ -> stabM pos c

instance StabM HoverM Expr Hover where
  stabM pos x = whenInRange' pos (locOf x) $ case x of
    Paren a        -> stabM pos a
    Var   a _      -> stabM pos a
    Const a _      -> stabM pos a
    Op op          -> stabM pos op
    Chain a op c _ -> do
      concat <$> sequence [stabM pos a, stabM pos op, stabM pos c]
    App a b _ -> (<>) <$> stabM pos a <*> stabM pos b
    Lam _ b _ -> stabM pos b
    Quant op _ c d _ ->
      concat <$> sequence [stabM pos op, stabM pos c, stabM pos d]
    _ -> return []

instance StabM HoverM Op Hover where
  stabM pos (ChainOp op) = stabM pos op
  stabM pos (ArithOp op) = stabM pos op
  stabM pos (QuantOp op) = stabM pos op

instance StabM HoverM ArithOp Hover where
  stabM pos op =
    whenInRange' pos (locOf op) $ return [typeToHover (Type.arithOpTypes op)]

instance StabM HoverM ChainOp Hover where
  stabM pos op =
    whenInRange' pos (locOf op) $ return [typeToHover (Type.chainOpTypes op)]

instance StabM HoverM QuantOp Hover where
  stabM pos op =
    whenInRange' pos (locOf op) $ return [typeToHover (Type.quantOpTypes op)]

instance StabM HoverM QuantOp' Hover where
  stabM pos (Left op) = stabM pos op
  stabM pos (Right expr) = stabM pos expr

instance StabM HoverM Name Hover where
  stabM pos name = if pos `stabbed'` name
    then do
      scopes <- ask
      return $ maybeToList $ lookupScopes scopes name
    else return []


instance StabM HoverM Program Hover where
  stabM pos (Program decls _ _ stmts l) =
    whenInRange' pos l $ (<>) <$> stabM pos decls <*> stabM pos stmts
