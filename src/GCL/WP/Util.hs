{-# LANGUAGE OverloadedStrings #-}

module GCL.WP.Util where

import           Control.Monad.Except           ( forM
                                                , unless
                                                )
import           Control.Monad.RWS              ( MonadReader(ask)
                                                , MonadWriter(..)
                                                , withRWST
                                                )

import qualified Data.Map                      as Map
import           Data.Loc                       ( Loc(..) )
import           Data.Loc.Range                 ( Range )
import qualified Data.Text                     as Text
import qualified Data.Hashable                 as Hashable
import           GCL.Predicate                  ( Origin(..)
                                                , PO(..)
                                                , Pred(..)
                                                , Spec(Specification)
                                                )
import           GCL.Predicate.Util             ( disjunct
                                                , guardIf
                                                , toExpr
                                                )
import           GCL.Common                     ( Fresh(..)
                                                , freshName'
                                                )

import qualified Syntax.Abstract               as A
import qualified Syntax.Abstract.Util          as A
import           Syntax.Common                  ( Name(..)
                                                , nameToText
                                                )
import qualified GCL.Substitution              as Substitution
import           Numeric                        ( showHex )
import           Pretty                         ( toString )
import           GCL.WP.Type

-- Syntax Manipulation

--- grouping a sequence of statement by assertions and specs

groupStmts :: [A.Stmt] -> [SegElm]
groupStmts []                            = []
groupStmts (s@(A.Assert _ _)    : stmts) = SAsrt s : groupStmts stmts
groupStmts (s@A.LoopInvariant{} : stmts) = SAsrt s : groupStmts stmts
groupStmts (s@(A.Spec _ _)      : stmts) = SSpec s : groupStmts stmts
groupStmts (s                   : stmts) = case groupStmts stmts of
  []                 -> [SStmts [s]]
  (SStmts ss : segs) -> SStmts (s : ss) : segs
  (s'        : segs) -> SStmts [s] : s' : segs

--- removing assertions (while keeping loop invariants).
--- succeed if there are no specs.

stripAsserts :: [A.Stmt] -> Maybe [A.Stmt]
stripAsserts []                     = Just []
stripAsserts (A.Assert _ _ : stmts) = stripAsserts stmts
stripAsserts (s1@A.LoopInvariant{} : s2@A.Do{} : stmts) =
  (s1 :) <$> stripAsserts (s2 : stmts)
stripAsserts (A.LoopInvariant{} : stmts) = stripAsserts stmts
stripAsserts (A.Spec _     _    : _    ) = Nothing
stripAsserts (A.If   gcmds l    : stmts) = do
  gcmds' <- forM gcmds $ \(A.GdCmd guard body l') -> do
    body' <- stripAsserts body
    return (A.GdCmd guard body' l')
  stmts' <- stripAsserts stmts
  return (A.If gcmds' l : stmts')
stripAsserts (A.Do gcmds l : stmts) = do
  gcmds' <- forM gcmds $ \(A.GdCmd guard body l') -> do
    body' <- stripAsserts body
    return (A.GdCmd guard body' l')
  stmts' <- stripAsserts stmts
  return (A.Do gcmds' l : stmts')
stripAsserts (s : stmts) = (s :) <$> stripAsserts stmts

disjunctGuards :: [A.GdCmd] -> Pred
disjunctGuards = disjunct . map guardIf . A.getGuards

-- PO handling

tellPO :: Pred -> Pred -> Origin -> WP ()
tellPO p q origin = unless (toExpr p == toExpr q) $ do
  p' <- substitute [] [] p
  q' <- substitute [] [] q
  let anchorHash =
        Text.pack $ showHex (abs (Hashable.hash (toString (p', q')))) ""
  tell ([PO p' q' anchorHash Nothing origin], [], [], mempty)


tellPO' :: Origin -> Pred -> Pred -> WP ()
tellPO' l p q = tellPO p q l

tellSpec :: Pred -> Pred -> Range -> WP ()
tellSpec p q l = do
  p' <- substitute [] [] p
  q' <- substitute [] [] q
  i  <- fresh
  tell ([], [Specification i p' q' l], [], mempty)

throwWarning :: StructWarning -> WP ()
throwWarning warning = tell ([], [], [warning], mempty)

-- ugly imports

substitute
  :: ( Substitution.Substitutable a
     , Substitution.Reducible a
     , Substitution.CollectRedexes a
     )
  => [Name]
  -> [A.Expr]
  -> a
  -> WP a
substitute xs es expr = do
  scope             <- ask
  (result, redexes) <- Substitution.run scope xs es expr
  tell ([], [], [], redexes)
  return result

-- Misc.

withFreshVar :: (A.Expr -> WP a) -> WP a
withFreshVar f = do
  name <- freshName' "bnd"
  let var = A.Var name NoLoc
  withRWST (\scope st -> (Map.insert (nameToText name) Nothing scope, st))
           (f var)
