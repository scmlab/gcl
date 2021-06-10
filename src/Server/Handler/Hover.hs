
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
      source       <- getSource
      program      <- parseProgram source
      hoverResults <- runHoverM program $ stabM pos program
      return $ listToMaybe $ map resultHover hoverResults

data HoverResult = Result
  { resultHover :: Hover
  , resultType  :: Type
  }

--------------------------------------------------------------------------------

-- | A "Scope" is a mapping of names and HoverResults
type Scope = Map Text HoverResult

-- | See if a name is in the scope
lookupScope :: Scope -> Name -> Maybe HoverResult
lookupScope scope name = Map.lookup (nameToText name) scope

-- | See if a name is in a series of scopes (from local to global)
-- | Return the first result (which should be the most local target)
lookupScopes :: [Scope] -> Name -> Maybe HoverResult
lookupScopes scopes name = foldl findFirst Nothing scopes
 where
  findFirst :: Maybe HoverResult -> Scope -> Maybe HoverResult
  findFirst (Just found) _     = Just found
  findFirst Nothing      scope = lookupScope scope name


--------------------------------------------------------------------------------

type HoverM = ReaderT [Scope] CmdM

runHoverM :: Program -> HoverM a -> CmdM a
runHoverM (Program decls _ _ _ _) = flip runReaderT [declScope]
 where
  declScope :: Map Text HoverResult
  declScope = case runExcept (foldM Type.inferDecl Map.empty decls) of
    -- ignore type errors 
    Left  _   -> Map.empty
    Right env -> Map.mapKeys nameToText $ Map.map typeToHoverResult env

typeToHoverResult :: Type -> HoverResult
typeToHoverResult t = Result { resultHover = hover, resultType = t }
 where
  hover   = Hover content Nothing
  content = HoverContents $ markedUpContent "gcl" (toText t)

instance StabM HoverM Stmt HoverResult where
  stabM pos x = whenInRange' pos (locOf x) $ case x of
    Assign a b _        -> (<>) <$> stabM pos a <*> stabM pos b
    Assert a _          -> stabM pos a
    LoopInvariant a b _ -> (<>) <$> stabM pos a <*> stabM pos b
    Do a _              -> stabM pos a
    If a _              -> stabM pos a
    _                   -> return []

instance StabM HoverM GdCmd HoverResult where
  stabM pos (GdCmd gd stmts l) =
    whenInRange' pos l $ (<>) <$> stabM pos gd <*> stabM pos stmts

instance StabM HoverM Declaration HoverResult where
  stabM pos x = whenInRange' pos (locOf x) $ case x of
    ConstDecl a    _    c    _ -> (<>) <$> stabM pos a <*> stabM pos c
    VarDecl   a    _    c    _ -> (<>) <$> stabM pos a <*> stabM pos c
    LetDecl   name args body _ -> do
      name' <- stabM pos name
      args' <- stabM pos (toArgs name args)
      body' <- stabM pos body
      return $ concat [name', args', body']

instance StabM HoverM Expr HoverResult where
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

instance StabM HoverM Op HoverResult where
  stabM pos (ChainOp op) = stabM pos op
  stabM pos (ArithOp op) = stabM pos op
  stabM pos (QuantOp op) = stabM pos op

instance StabM HoverM ArithOp HoverResult where
  stabM pos op = whenInRange' pos (locOf op)
    $ return [typeToHoverResult (Type.arithOpTypes op)]

instance StabM HoverM ChainOp HoverResult where
  stabM pos op = whenInRange' pos (locOf op)
    $ return [typeToHoverResult (Type.chainOpTypes op)]

instance StabM HoverM QuantOp HoverResult where
  stabM pos op = whenInRange' pos (locOf op)
    $ return [typeToHoverResult (Type.quantOpTypes op)]

instance StabM HoverM QuantOp' HoverResult where
  stabM pos (Left  op  ) = stabM pos op
  stabM pos (Right expr) = stabM pos expr

instance StabM HoverM Name HoverResult where
  stabM pos name = if pos `stabbed'` name
    then do
      scopes <- ask
      return $ maybeToList $ lookupScopes scopes name
    else return []

instance StabM HoverM Program HoverResult where
  stabM pos (Program decls _ _ stmts l) =
    whenInRange' pos l $ (<>) <$> stabM pos decls <*> stabM pos stmts


--------------------------------------------------------------------------------

-- | A datatype for representing an argument of a function like definition 
data Arg = Arg Name -- Name of the function  
                    Name -- Name of the argument
                         Int -- Index of the argument (starts from 0)


toArgs :: Name -> [Name] -> [Arg]
toArgs func args = zipWith (Arg func) args [0 ..]

instance StabM HoverM Arg HoverResult where
  stabM pos (Arg func arg index) = whenInRange' pos (locOf arg) $ do
    scopes <- ask
    case lookupScopes scopes func of
      Nothing -> return []
      Just (Result _ t) ->
        return $ map typeToHoverResult $ maybeToList $ locateArgType t index

   where
      -- given the type of a function and the index of one of its argument
      -- return the type of that argument
    locateArgType :: Type -> Int -> Maybe Type
    locateArgType (TFunc argType _        _) 0 = Just argType
    locateArgType (TFunc _       bodyType _) n = locateArgType bodyType (n - 1)
    locateArgType _otherType                 _ = Nothing
