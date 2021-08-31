module Syntax.Abstract.Util where

import Syntax.Abstract
    ( Expr(Lam), GdCmd(..), Stmt, Declaration(..), QDCon (..), Bindings (..), Type (..), QTyCon (..))
import Syntax.Common (Name)
import Data.Loc ((<-->))
import Data.Map (Map)
import qualified Data.Map as Map

extractAssertion :: Declaration -> Maybe Expr
extractAssertion (ConstDecl _ _ e _) = e
extractAssertion (VarDecl _ _ e _) = e
extractAssertion LetDecl {} = Nothing
extractAssertion TypeDecl {} = Nothing

extractLetBinding :: Declaration -> Maybe (Name, Expr)
extractLetBinding ConstDecl {} = Nothing
extractLetBinding VarDecl {} = Nothing
extractLetBinding (LetDecl name args expr _) = Just (name, wrapLam args expr)
-- TODO:
extractLetBinding TypeDecl {} = Nothing

extractTypeDecls :: Declaration -> Maybe Type
extractTypeDecls (TypeDecl (QTyCon n args) _ _) = Just (TCon (QTyCon n args))
extractTypeDecls _ = Nothing

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
extractDeclaration (LetDecl n args body _) = Map.singleton n (Just (wrapLam args body))
-- extract type constructor to env
extractDeclaration (TypeDecl _ cons _) = Map.fromList $ map (\(QDCon n _) -> (n, Nothing)) cons

extractDeclarations :: [Declaration] -> Map Name (Maybe Expr)
extractDeclarations = foldl (\m decl -> m `Map.union` extractDeclaration decl) mempty
