module Syntax.Abstract.Util where

import           Data.Loc                       ( (<-->)
                                                , Located(locOfList)
                                                , locOf
                                                )
import           Data.Map                       ( Map )
import qualified Data.Map                      as Map
import           Data.Text                      ( Text )
import           Syntax.Abstract
import           Syntax.Common                  ( Name
                                                , nameToText
                                                )

extractAssertion :: Declaration -> Maybe Expr
extractAssertion (ConstDecl _ _ e _) = e
extractAssertion (VarDecl   _ _ e _) = e

extractTypeDefnCtors :: TypeDefn -> [Declaration]
extractTypeDefnCtors (TypeDefn name binders qdcons _) = map wrap qdcons
 where
  wrap (TypeDefnCtor cn ts) = ConstDecl
    [cn]
    (wrapTFunc ts (TCon name binders (locOf name <--> locOfList binders)))
    Nothing
    (locOf cn)

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

-- function definition           => Just Expr 
-- constant/variable declaration => Nothing 
programToScopeForSubstitution :: Program -> Map Text (Maybe Expr)
programToScopeForSubstitution (Program (Defns _ funcDefns) decls _ _ _) =
  Map.mapKeys nameToText
    $  foldMap extractDeclaration decls
    <> Map.map Just funcDefns
 where
  extractDeclaration :: Declaration -> Map Name (Maybe Expr)
  extractDeclaration (ConstDecl names _ _ _) =
    Map.fromList (zip names (repeat Nothing))
  extractDeclaration (VarDecl names _ _ _) =
    Map.fromList (zip names (repeat Nothing))


    --extractDeclaration (LetDecl n args body _) = Map.singleton n (Just (wrapLam args body))
