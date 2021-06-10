
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}


{-# LANGUAGE FlexibleInstances #-}
module Server.Handler.Hover (handler) where

import Server.Stab
import Syntax.Abstract
import Server.Interpreter.RealWorld
import Error (Error)

import           Language.LSP.Types      hiding ( Range )
import Server.DSL
import Control.Monad.Reader
import Data.Text (Text)
import Data.Map (Map)
import qualified Data.Map as Map
import Syntax.Common
import Data.Loc (locOf)
import qualified GCL.Type as Type
import Control.Monad.Except
import Data.Maybe (listToMaybe, maybeToList)
import Pretty (toText)


ignoreErrors :: Either [Error] (Maybe Hover) -> Maybe Hover
ignoreErrors (Left  _errors  ) = Nothing
ignoreErrors (Right locations) = locations

handler :: Uri -> Position -> (Maybe Hover -> ServerM ()) -> ServerM ()
handler uri pos responder =
  case uriToFilePath uri of
    Nothing       -> return ()
    Just filepath -> do
      interpret filepath (responder . ignoreErrors) $ do
        source  <- getSource
        program <- parseProgram source
        hovers <- runHoverM program $ stabM pos program
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
    Left _ -> Map.empty
    Right env -> Map.mapKeys nameToText $ Map.map typeToHover env

  typeToHover :: Type -> Hover
  typeToHover t = Hover content Nothing
    where
      content = HoverContents $ markedUpContent "gcl" (toText t)
  -- (Just (J.toRange (rangeOf t)))


    -- Map.fromList (decls >>= mapMaybe declToLocationLink . splitDecl)

  -- env <- foldM Type.inferDecl emptyEnv decls



-- handler :: J.Uri -> J.Position -> ServerM (Maybe J.Hover)
-- handler uri position = do
--   (_, result) <- parseAndScopeCheck uri
--   case result of
--     Nothing -> return Nothing
--     Just program -> return $ stabMaybe position program

instance StabM HoverM Program Hover where
  stabM pos (Program decls _ _ stmts l) = whenInRange' pos l $
    (<>) <$> stabM pos decls <*> stabM pos stmts

instance StabM HoverM Stmt Hover where
  stabM pos x = whenInRange' pos (locOf x) $ case x of 
    Assign a b _        -> (<>) <$> stabM pos a <*> stabM pos b
    Assert a _          -> stabM pos a
    LoopInvariant a b _ -> (<>) <$> stabM pos a <*> stabM pos b
    Do a _              -> stabM pos a
    If a _              -> stabM pos a
    _                   -> return []

instance StabM HoverM GdCmd Hover where
  stabM pos (GdCmd gd stmts l) = whenInRange' pos l $ (<>) <$> stabM pos gd <*> stabM pos stmts

instance StabM HoverM Declaration Hover where
  stabM pos x = whenInRange' pos (locOf x) $ case x of
    ConstDecl _ _ c _ -> stabM pos c
    VarDecl   _ _ c _ -> stabM pos c
    LetDecl   _ _ c _ -> stabM pos c

instance StabM HoverM Expr Hover where
  stabM pos x = whenInRange' pos (locOf x) $ case x of
    Var   a _       -> stabM pos a
    Const a _       -> stabM pos a
    Paren a         -> stabM pos a
    Chain a _ c _   -> (<>) <$> stabM pos a <*> stabM pos c
    App a b _       -> (<>) <$> stabM pos a <*> stabM pos b
    Lam _ b _       -> stabM pos b
    Quant _ _ c d _ -> (<>) <$> stabM pos c <*> stabM pos d
    _               -> return []

  -- = Paren Expr
  -- | Lit Lit Loc
  -- | Var Name Loc
  -- | Const Name Loc
  -- | Op ArithOp
  -- | Chain Expr ChainOp Expr Loc
  -- | App Expr Expr Loc
  -- | Lam Name Expr Loc
  -- | Hole Loc
  -- | Quant QuantOp' [Name] Expr Expr Loc
  -- | Subst Expr Subst Expr

instance StabM HoverM Name Hover where
  stabM pos name =
    if pos `stabbed'` name
      then do
        scopes <- ask
        return $ maybeToList $ lookupScopes scopes name
      else return []
