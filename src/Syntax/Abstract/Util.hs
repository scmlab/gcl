module Syntax.Abstract.Util where

import           Data.Loc                       ( (<-->)
                                                , locOf
                                                )
import           Data.Map                       ( Map )
import qualified Data.Map                      as Map
import           Syntax.Abstract
import           Syntax.Common                  ( Name )

extractAssertion :: Declaration -> Maybe Expr
extractAssertion (ConstDecl _ _ e _) = e
extractAssertion (VarDecl   _ _ e _) = e

extractQDCons :: TypeDefn -> [Declaration]
extractQDCons (TypeDefn qty qdcons _) = map wrap qdcons
 where
  wrap (QDCon cn ts) =
    ConstDecl [cn] (wrapTFunc ts (TCon qty)) Nothing (locOf cn)

wrapTFunc :: [Type] -> Type -> Type
wrapTFunc []       t  = t
wrapTFunc [t     ] t0 = TFunc t t0 (locOf t)
wrapTFunc (t : ts) t0 = let t0' = wrapTFunc ts t0 in TFunc t t0' (t <--> t0')

getGuards :: [GdCmd] -> [Expr]
getGuards = fst . unzipGdCmds

unzipGdCmds :: [GdCmd] -> ([Expr], [[Stmt]])
unzipGdCmds = unzip . map (\(GdCmd x y _) -> (x, y))

wrapLam :: [Name] -> Expr -> Expr
wrapLam []       body = body
wrapLam (x : xs) body = let b = wrapLam xs body in Lam x b (x <--> b)

bindingsToExpr :: Bindings -> Expr
bindingsToExpr (AssignBinding e) = e
bindingsToExpr (LetBinding    e) = e
bindingsToExpr (BetaBinding   e) = e
bindingsToExpr (AlphaBinding  e) = e

assignBindingToExpr :: Bindings -> Maybe Expr
assignBindingToExpr (AssignBinding e) = Just e
assignBindingToExpr _                 = Nothing

extractDeclaration :: Declaration -> Map Name (Maybe Expr)
extractDeclaration (ConstDecl ns _ _ _) =
  Map.fromList (zip ns (repeat Nothing))
extractDeclaration (VarDecl ns _ _ _) = Map.fromList (zip ns (repeat Nothing))
--extractDeclaration (LetDecl n args body _) = Map.singleton n (Just (wrapLam args body))

-- extract type constructor to env
--extractDeclaration (TypeDefn _ cons _) = Map.fromList $ map (\(QDCon n _) -> (n, Nothing)) cons

extractDeclarations :: [Declaration] -> Defns -> Map Name (Maybe Expr)
extractDeclarations decls (Defns _ funcDefns) =
  foldMap extractDeclaration decls <> Map.map Just funcDefns