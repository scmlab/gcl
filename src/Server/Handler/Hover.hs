
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}


{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
module Server.Handler.Hover
  ( handler
  ) where

import           Error                          ( Error )
import           Server.Interpreter.RealWorld
import           Server.Stab
import           Syntax.Abstract

import           Control.Monad.Except
import           Control.Monad.Reader
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
import Render
import Data.Loc (locOf, Located)

ignoreErrors :: Either [Error] (Maybe Hover) -> Maybe Hover
ignoreErrors (Left  _errors  ) = Nothing
ignoreErrors (Right locations) = locations

handler :: Uri -> Position -> (Maybe Hover -> ServerM ()) -> ServerM ()
handler uri pos responder = case uriToFilePath uri of
  Nothing       -> return ()
  Just filepath -> do
    interpret filepath (responder . ignoreErrors) $ do
      source       <- getSource
      program      <- parseProgram source
      hoverResults <- runHoverM program pos $ stabM program
      return $ listToMaybe $ map resultHover hoverResults

data HoverResult = Result
  { resultHover :: Hover
  , resultType  :: Type
  }
  deriving Show

instance Render HoverResult where 
  render (Result _ t) = "Hover " <+> render t

--------------------------------------------------------------------------------

-- -- | A "Scope" is a mapping of names and HoverResults
-- type Scope = Map Text HoverResult

-- -- | See if a name is in the scope
-- lookupScope :: Scope -> Name -> Maybe HoverResult
-- lookupScope scope name = Map.lookup (nameToText name) scope

-- -- | See if a name is in a series of scopes (from local to global)
-- -- | Return the first result (which should be the most local target)
-- lookupScopes :: [Scope] -> Name -> Maybe HoverResult
-- lookupScopes scopes name = foldl findFirst Nothing scopes
--  where
--   findFirst :: Maybe HoverResult -> Scope -> Maybe HoverResult
--   findFirst (Just found) _     = Just found
--   findFirst Nothing      scope = lookupScope scope name


--------------------------------------------------------------------------------

type HoverM = ReaderT Position (ReaderT [Scope HoverResult] CmdM)

instance HasPosition HoverM where
  askPosition = ask

instance HasScopes HoverM HoverResult where
  askScopes = lift ask
  pushScope scope p = do
    pos <- ask
    lift $ local (scope :) $ runReaderT p pos

runHoverM :: Program -> Position -> HoverM a -> CmdM a
runHoverM (Program decls _ _ _ _) pos f = runReaderT (runReaderT f pos)
                                                     [declScope]
 where
  declScope :: Map Text HoverResult
  declScope = case Type.runTM (Type.declsToEnv decls) of
    -- ignore type errors
    Left  _   -> Map.empty
    Right env -> Map.mapKeys nameToText $ Map.map typeToHoverResult (Type.localDecls env)

typeToHoverResult :: Type -> HoverResult
typeToHoverResult t = Result { resultHover = hover, resultType = t }
 where
  hover   = Hover content Nothing
  content = HoverContents $ markedUpContent "gcl" (toText t)

instance StabM HoverM Stmt HoverResult where
  stabM = \case
    Assign a b _        -> (<>) <$> stabLocated a <*> stabLocated b
    Assert a _          -> stabLocated a
    LoopInvariant a b _ -> (<>) <$> stabLocated a <*> stabLocated b
    Do a _              -> stabLocated a
    If a _              -> stabLocated a
    _                   -> return []

instance StabM HoverM GdCmd HoverResult where
  stabM (GdCmd gd stmts _) = (<>) <$> stabLocated gd <*> stabLocated stmts

instance StabM HoverM Declaration HoverResult where
  stabM = \case
    ConstDecl a    _    c    _ -> (<>) <$> stabLocated a <*> stabLocated c
    VarDecl   a    _    c    _ -> (<>) <$> stabLocated a <*> stabLocated c
    LetDecl   name args body _ -> do
      name' <- stabLocated name
      -- creates a local scope for arguments
      args' <- stabLocated (toArgs name args)
      let argsScope = Map.fromList $ zip (map nameToText args) args'
      -- temporarily prepend this local scope to the scope stack
      body' <- pushScope argsScope $ stabLocated body

      return $ concat [name', args', body']
    -- TODO: hover type declaration
    TypeDecl {} -> return mempty

instance StabM HoverM Expr HoverResult where
  stabM = \case
    -- Paren a _      -> stabLocated a
    Var   a _      -> stabLocated a
    Const a _      -> stabLocated a
    Op op          -> stabLocated op
    Chain a op c _ -> do
      concat <$> sequence [stabLocated a, stabLocated op, stabLocated c]
    App a b _ -> (<>) <$> stabLocated a <*> stabLocated b
    Lam _ b _ -> stabLocated b
    Quant op _ c d _ ->
      concat <$> sequence [stabLocated op, stabLocated c, stabLocated d]
    _ -> return []

instance StabM HoverM Op HoverResult where
  stabM (ChainOp op) = stabLocated op
  stabM (ArithOp op) = stabLocated op

instance StabM HoverM ArithOp HoverResult where
  stabM op = return [typeToHoverResult (Type.arithOpTypes op)]

instance StabM HoverM ChainOp HoverResult where
  stabM op = return [typeToHoverResult (Type.chainOpTypes op)]

instance StabM HoverM QuantOp' HoverResult where
  stabM (Left  op  ) = stabLocated op
  stabM (Right expr) = stabLocated expr

instance StabM HoverM Name HoverResult where
  stabM name = do
    result <- lookupScopes (nameToText name)
    case result of
      Nothing          -> return []
      Just hoverResult -> return [hoverResult]

instance StabM HoverM Program HoverResult where
  stabM (Program decls _ _ stmts _) =
    (<>) <$> stabLocated decls <*> stabLocated stmts


--------------------------------------------------------------------------------

-- | A datatype for representing an argument of a function like definition
data Arg = Arg Name -- Name of the function
                    Name -- Name of the argument
                         Int -- Index of the argument (starts from 0)

instance Located Arg where 
  locOf (Arg _ arg _) = locOf arg 

toArgs :: Name -> [Name] -> [Arg]
toArgs func args = zipWith (Arg func) args [0 ..]

instance StabM HoverM Arg HoverResult where
  stabM (Arg func _arg index) = do
    result <- lookupScopes (nameToText func)
    case result of
      Nothing -> return []
      Just (Result _ t) ->
        return $ map typeToHoverResult $ maybeToList $ locateArgType t index

    -- scopes <- askScopes
    -- case lookupScopes scopes func of
    --   Nothing -> return []
    --   Just (Result _ t) ->
    --     return $ map typeToHoverResult $ maybeToList $ locateArgType t index

   where
      -- given the type of a function and the index of one of its argument
      -- return the type of that argument
    locateArgType :: Type -> Int -> Maybe Type
    locateArgType (TFunc argType _        _) 0 = Just argType
    locateArgType (TFunc _       bodyType _) n = locateArgType bodyType (n - 1)
    locateArgType _otherType                 _ = Nothing
