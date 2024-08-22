{-# LANGUAGE OverloadedStrings #-}

module Syntax.Abstract.Util where

import           Data.Loc                       ( (<-->)
                                                , locOf, Loc (NoLoc)
                                                )
import           Data.Map                       ( Map )
import qualified Data.Map                      as Map
import qualified Data.Maybe                    as Maybe
import qualified Data.List                     as List
import           Data.Text                      ( Text )
import           Data.Bifunctor                 ( second )
import           Syntax.Abstract
import           Syntax.Common                  ( Name(..)
                                                , nameToText
                                                , TypeOp(Arrow)
                                                )

--funcDefnSigsToConstDecl :: FuncDefnSig -> [Declaration]
--funcDefnSigsToConstDecl (FuncDefnSig name t prop loc) =
  --[ConstDecl [name] t prop loc]

--typeDefnCtorsToConstDecl :: TypeDefn -> [Declaration]
--typeDefnCtorsToConstDecl (TypeDefn name binders qdcons _) = map wrap qdcons
 --where
  --wrap (TypeDefnCtor cn ts) = ConstDecl
    --[cn]
    --(wrapTFunc ts (TCon name binders (locOf name <--> locOfList binders)))
    --Nothing
    --(locOf cn)

wrapTFunc :: [Type] -> Type -> Type
wrapTFunc []       t  = t
wrapTFunc (t : ts) t0 = let t0' = wrapTFunc ts t0 in TApp (TApp (TOp (Arrow NoLoc)) t NoLoc) t0' (locOf t0) -- TODO: What should the loc be?

getGuards :: [GdCmd] -> [Expr]
getGuards = fst . unzipGdCmds

unzipGdCmds :: [GdCmd] -> ([Expr], [[Stmt]])
unzipGdCmds = unzip . map (\(GdCmd x y _) -> (x, y))

wrapLam :: [Name] -> Expr -> Expr
wrapLam []       body = body
wrapLam (x : xs) body = let b = wrapLam xs body in Lam x b (x <--> b)

declaredNames :: [Declaration] -> [Name]
declaredNames decls = concat . map extractNames $ decls
  where extractNames (ConstDecl ns _ _ _) = ns
        extractNames (VarDecl   ns _ _ _) = ns

-- function definition           => Just Expr
-- constant/variable declaration => Nothing

-- TODO:
programToScopeForSubstitution :: Program -> Map Text (Maybe Expr)
programToScopeForSubstitution (Program defns decls _ _ _) =
  Map.mapKeys nameToText
    $  foldMap extractDeclaration decls
    <> ( Map.fromList
       . map (second Just)
       . Maybe.mapMaybe pickFuncDefn
       )
         defns

 where
  extractDeclaration :: Declaration -> Map Name (Maybe Expr)
  extractDeclaration (ConstDecl names _ _ _) =
    Map.fromList (zip names (repeat Nothing))
  extractDeclaration (VarDecl names _ _ _) =
    Map.fromList (zip names (repeat Nothing))

pickFuncDefn :: Definition -> Maybe (Name, Expr)
pickFuncDefn (FuncDefn n expr) = Just (n, expr)
pickFuncDefn _                 = Nothing

{-
combineFuncDefns :: [Definition] -> [Definition]
combineFuncDefns defns =
  let (funcDefns, otherDefns) =
        List.partition (Maybe.isJust . pickFuncDefn) defns
  in  let combinedFuncDefns = map (uncurry FuncDefn) . Map.toList $ foldl
            (\m (FuncDefn n es) -> Map.insertWith (<>) n es m)
            mempty
            funcDefns
      in  combinedFuncDefns <> otherDefns
-}
--collectFuncDefns = Map.fromListWith mergeFuncDefnsOfTheSameName
  -- . map (\(FuncDefn name clauses _) -> (name, map (uncurry wrapLam) clauses))
 --where
  --mergeFuncDefnsOfTheSameName :: [Expr] -> [Expr] -> [Expr]
  --mergeFuncDefnsOfTheSameName = (<>)

baseToName :: TBase -> Name
baseToName TInt  = Name "Int" NoLoc
baseToName TBool = Name "Bool" NoLoc
baseToName TChar = Name "Char" NoLoc
