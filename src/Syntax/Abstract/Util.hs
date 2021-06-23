module Syntax.Abstract.Util where

import Syntax.Abstract
    ( Expr(Lam), GdCmd(..), Stmt, Declaration(..) )
import Syntax.Abstract.Located()
import Syntax.Common (Name)
import Data.Loc ((<-->))

extractAssertion :: Declaration -> Maybe Expr
extractAssertion (ConstDecl _ _ e _) = e
extractAssertion (VarDecl _ _ e _) = e
extractAssertion LetDecl {} = Nothing

extractDeclaration :: Declaration -> Maybe Declaration 
extractDeclaration d@ConstDecl {} = Just d
extractDeclaration d@VarDecl {} = Just d
extractDeclaration _ = Nothing

extractLetBinding :: Declaration -> Maybe (Name, Expr)
extractLetBinding ConstDecl {} = Nothing
extractLetBinding VarDecl {} = Nothing
extractLetBinding (LetDecl name args expr _) = Just (name, wrapLam args expr)

getGuards :: [GdCmd] -> [Expr]
getGuards = fst . unzipGdCmds

unzipGdCmds :: [GdCmd] -> ([Expr], [[Stmt]])
unzipGdCmds = unzip . map (\(GdCmd x y _) -> (x, y))

wrapLam :: [Name] -> Expr -> Expr
wrapLam [] body = body
wrapLam (x : xs) body = 
  let b = wrapLam xs body in
  Lam x b (x <--> b)
