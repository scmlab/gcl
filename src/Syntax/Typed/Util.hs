{-# LANGUAGE OverloadedStrings #-}

module Syntax.Typed.Util where

import           Control.Arrow                  ( second )
import           Data.Map                       ( Map )
import qualified Data.Map                      as Map
import qualified Data.Maybe                    as Maybe
import           Data.Text                      ( Text )
import           Data.Loc                       ( (<-->), Loc(..) )
import           Syntax.Typed
import           Syntax.Common                  ( Name(..), nameToText )
import           Syntax.Abstract.Types          ( Type(..), TBase (TBool) )

getGuards :: [GdCmd] -> [Expr]
getGuards = fst . unzipGdCmds

unzipGdCmds :: [GdCmd] -> ([Expr], [[Stmt]])
unzipGdCmds = unzip . map (\(GdCmd x y _) -> (x, y))

wrapLam :: [(Name, Type)] -> Expr -> Expr
wrapLam []           body = body
wrapLam ((x,t) : xs) body = let b = wrapLam xs body in Lam x t b (x <--> b)

declaredNames :: [Declaration] -> [Name]
declaredNames decls = concat . map extractNames $ decls
  where extractNames (ConstDecl ns _ _ _) = ns
        extractNames (VarDecl   ns _ _ _) = ns

declaredNamesTypes :: [Declaration] -> [(Name, Type)]
declaredNamesTypes decls = concat . map extractNames $ decls
  where extractNames (ConstDecl ns t _ _) = [(n, t) | n <- ns]
        extractNames (VarDecl   ns t _ _) = [(n, t) | n <- ns]

typeOf :: Expr -> Type
typeOf (Lit _ t _)   = t
typeOf (Var _ t _)   = t
typeOf (Const _ t _) = t
typeOf (Op _ t) = t
typeOf (Chain ch) = typeOfChain ch
typeOf (App e0 _ _) = case typeOf e0 of
  Syntax.Abstract.Types.TFunc _ t _ -> t
  _ -> error "left term not having function type in a typed expression"
typeOf (Lam _ t0 e _) = Syntax.Abstract.Types.TFunc t0 (typeOf e) NoLoc
typeOf (Quant _ _ _ body _) = typeOf body
typeOf (ArrIdx arr _ _) = case typeOf arr of
  Syntax.Abstract.Types.TArray _ t _ -> t
  Syntax.Abstract.Types.TFunc  _ t _ -> t
  _ -> error "indexed term not an array in a typed expression "
typeOf (ArrUpd arr _ _ _) = typeOf arr
typeOf (Case expr _ _) = typeOf expr -- FIXME: This is wrong and acts as a placeholder. Figure out what this should be.
typeOf (Subst e _) = typeOf e

typeOfChain :: Chain -> Type
typeOfChain (Pure e) = typeOf e  -- SCM: shouldn't happen?
typeOfChain (More _ _ _ _) = Syntax.Abstract.Types.TBase TBool NoLoc


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
