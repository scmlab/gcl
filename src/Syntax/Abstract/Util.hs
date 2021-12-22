{-# LANGUAGE OverloadedStrings #-}

module Syntax.Abstract.Util where

import           Data.Loc                       ( (<-->)
                                                , Loc(..)
                                                , Located(locOfList)
                                                , locOf
                                                )
import           Data.Map                       ( Map )
import qualified Data.Map                      as Map
import qualified Data.Maybe                    as Maybe
import           Data.Text                      ( Text )
import           Syntax.Abstract
import           Syntax.Common                  ( Name(..)
                                                , nameToText
                                                )

funcDefnSigsToConstDecl :: FuncDefnSig -> [Declaration]
funcDefnSigsToConstDecl (FuncDefnSig name t prop loc) =
  [ConstDecl [name] t prop loc]

typeDefnCtorsToConstDecl :: TypeDefn -> [Declaration]
typeDefnCtorsToConstDecl (TypeDefn name binders qdcons _) = map wrap qdcons
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

-- TODO:
programToScopeForSubstitution :: Program -> Map Text (Maybe Expr)
programToScopeForSubstitution (Program defns decls _ _ _) =
  Map.mapKeys nameToText $ foldMap extractDeclaration decls <> Map.map
    Maybe.listToMaybe
    (defnFuncs defns)
 where
  extractDeclaration :: Declaration -> Map Name (Maybe Expr)
  extractDeclaration (ConstDecl names _ _ _) =
    Map.fromList (zip names (repeat Nothing))
  extractDeclaration (VarDecl names _ _ _) =
    Map.fromList (zip names (repeat Nothing))

-- merge multiple FuncDefnClause into one
mergeFuncDefnClauses :: [FuncDefnClause] -> Map Name [Expr]
mergeFuncDefnClauses = Map.fromListWith mergeFuncDefnsOfTheSameName
  . map (\(FuncDefnClause name args body _) -> (name, [wrapLam args body]))
 where
  mergeFuncDefnsOfTheSameName :: [Expr] -> [Expr] -> [Expr]
  mergeFuncDefnsOfTheSameName = (<>)

baseToName :: TBase -> Name
baseToName TInt  = Name "Int" NoLoc
baseToName TBool = Name "Bool" NoLoc
baseToName TChar = Name "Char" NoLoc
