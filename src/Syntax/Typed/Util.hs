{-# LANGUAGE OverloadedStrings #-}

module Syntax.Typed.Util where

import           Data.Loc                       ( (<-->) )
import           Syntax.Typed
import           Syntax.Common                  ( Name(..) )


getGuards :: [GdCmd] -> [Expr]
getGuards = fst . unzipGdCmds

unzipGdCmds :: [GdCmd] -> ([Expr], [[Stmt]])
unzipGdCmds = unzip . map (\(GdCmd x y _) -> (x, y))

wrapLam :: [(Name, Type)] -> Expr -> Expr
wrapLam []           body = body
wrapLam ((x,t) : xs) body = let b = wrapLam xs body in Lam x t b (x <--> b)
