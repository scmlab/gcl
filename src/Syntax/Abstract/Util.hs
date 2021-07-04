module Syntax.Abstract.Util where

import Syntax.Abstract
    ( Expr(Lam), GdCmd(..), Stmt, Declaration(..), Bindings (..), DeclBody (..) )
import Syntax.Common (Name)
import Data.Loc ((<-->))
import Data.Map (Map)
import qualified Data.Map as Map

extractAssertion :: Declaration -> Maybe Expr
extractAssertion (ConstDecl _ _ e _) = e
extractAssertion (VarDecl _ _ e _) = e
extractAssertion LetDecl {} = Nothing
extractAssertion (BlockDecl _ _ e _ _) = e

extractLetBinding :: Declaration -> Maybe (Name, Expr)
extractLetBinding ConstDecl {} = Nothing
extractLetBinding VarDecl {} = Nothing
extractLetBinding (LetDecl (DeclBody name args expr) _) = Just (name, wrapLam args expr)
extractLetBinding BlockDecl {} = Nothing

getGuards :: [GdCmd] -> [Expr]
getGuards = fst . unzipGdCmds

unzipGdCmds :: [GdCmd] -> ([Expr], [[Stmt]])
unzipGdCmds = unzip . map (\(GdCmd x y _) -> (x, y))

wrapLam :: [Name] -> Expr -> Expr
wrapLam [] body = body
wrapLam (x : xs) body =
  let b = wrapLam xs body in
  Lam x b (x <--> b)

bindingsToExpr :: Bindings -> Expr
bindingsToExpr (AssignBinding e) = e
bindingsToExpr (LetBinding e) = e
bindingsToExpr (BetaBinding e) = e
bindingsToExpr (AlphaBinding e) = e

assignBindingToExpr :: Bindings -> Maybe Expr
assignBindingToExpr (AssignBinding e) = Just e
assignBindingToExpr _ = Nothing

extractDeclaration :: Declaration -> Map Name (Maybe Expr)
extractDeclaration (ConstDecl ns _ _ _) = Map.fromList (zip ns (repeat Nothing))
extractDeclaration (VarDecl ns _ _ _) = Map.fromList (zip ns (repeat Nothing))
extractDeclaration (LetDecl (DeclBody n args body) _) = Map.singleton n (Just (wrapLam args body))
extractDeclaration (BlockDecl ns _ _ ds _) = 
  Map.fromList (map (\(DeclBody n args body) -> (n, Just (wrapLam args body))) ds) 
    `Map.union` Map.fromList (zip ns (repeat Nothing))

extractDeclarations :: [Declaration] -> Map Name (Maybe Expr)
extractDeclarations = foldl (\m decl -> m `Map.union` extractDeclaration decl) mempty

-- extractIdentifier :: Declaration -> [Name]
-- extractIdentifier (ConstDecl ns _ _ _) = ns
-- extractIdentifier (VarDecl ns _ _ _) = ns
-- extractIdentifier (LetDecl n _ _ _) = [n]

-- globalIdentifiers :: [Declaration] -> [Name]
-- globalIdentifiers = concatMap extractIdentifier