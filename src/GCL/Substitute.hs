{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}

module GCL.Substitute where

import           Data.Loc                       ( locOf )
import           Data.Map                       ( Map )
import qualified Data.Map                      as Map
import           Data.Text                      ( Text )
import           GCL.Predicate                  ( Pred(..) )
import           Pretty
import           Syntax.Abstract                ( Bindings(LetBinding)
                                                , Expr(..)
                                                , Reason(..)
                                                )
import           Syntax.Abstract.Util           ( bindingsToExpr )
import           Syntax.Common                  ( Name
                                                , nameToText
                                                )

------------------------------------------------------------------

reducePred :: [Scope] -> Pred -> Pred
reducePred scopes x = case x of
    Constant a    -> Constant (reduceExpr scopes a)
    GuardIf   a l -> GuardIf (reduceExpr scopes a) l
    GuardLoop a l -> GuardLoop (reduceExpr scopes a) l
    Assertion a l -> Assertion (reduceExpr scopes a) l
    LoopInvariant a b l ->
        LoopInvariant (reduceExpr scopes a) (reduceExpr scopes b) l
    Bound a l   -> Bound (reduceExpr scopes a) l
    Conjunct as -> Conjunct (map (reducePred scopes) as)
    Disjunct as -> Disjunct (map (reducePred scopes) as)
    Negate   a  -> Negate (reducePred scopes a)

reduceExpr :: [Scope] -> Expr -> Expr
reduceExpr scopes = extract . reduce scopes . Value

------------------------------------------------------------------
extract :: Reason -> Expr
extract (ExpandContinue _ x              ) = extract x
extract (ExpandPause reasons before after) = Expand reasons before after
extract (ExpandStuck name                ) = Const name (locOf name)
extract (Congruence _ _ after            ) = extract after
extract (Value x                         ) = x

-- reduce' :: [Scope] -> Expr -> (Reason, Expr)
-- reduce' scopes expr =
--     let reason = reduceValue scopes expr in (reason, extract reason)

reduce :: [Scope] -> Reason -> Reason
reduce scopes expr = case expr of
    Value val -> reduceValue scopes val
    others    -> others

reduceValue :: [Scope] -> Expr -> Reason
reduceValue scopes expr = case expr of
    Paren e l ->
        let e' = reduce scopes (Value e)
        in  Congruence [e'] expr (Value $ Paren (extract e') l)

    Var name _ -> case lookupScopes scopes name of
        Nothing ->
            error
                $  "panic: "
                ++ show (nameToText name)
                ++ " is not in scope: "
                ++ show (pretty scopes)
        Just (UserDefinedBinding binding) -> ExpandPause [] expr binding
        Just (SubstitutionBinding binding) ->
            let scopes' = Map.singleton (nameToText name) NoBinding : scopes
            in  ExpandContinue name (reduce scopes' binding)
            -- traceShow ("Var SubstitutionBinding", pretty binding) $ ExpandContinue name binding 
        Just NoBinding -> ExpandStuck name

    Const name _ -> case lookupScopes scopes name of
        Nothing ->
            error
                $  "panic: "
                ++ show (nameToText name)
                ++ " is not in scope: "
                ++ show (pretty scopes)
        Just (UserDefinedBinding binding) -> ExpandPause [] expr binding
        Just (SubstitutionBinding binding) ->
            ExpandContinue name (reduce scopes binding)
        Just NoBinding -> ExpandStuck name
    Chain a op b l ->
        let a' = reduce scopes (Value a)
            b' = reduce scopes (Value b)
        in  Congruence [a', b'] expr $ Value $ Chain (extract a')
                                                     op
                                                     (extract b')
                                                     l
    App a b l ->
        let
            a' = reduce scopes (Value a)
            b' = reduce scopes (Value b)
        in
            case extract a' of
                Expand _ _ (Lam n x _) ->
                    let
                        scopes' =
                            Map.singleton (nameToText n)
                                          (SubstitutionBinding b')
                                : scopes
                        after  = App (extract a') (extract b') l
                        reason = reduceValue scopes' x
                    in
                        Congruence [a', b'] expr
                            $ ExpandPause [reason] after (extract reason)
                -- "App a' b'" is a redex
                Lam n x _ ->
                    let
                        scopes' =
                            Map.singleton (nameToText n)
                                          (SubstitutionBinding b')
                                : scopes
                    in  Congruence [a', b'] expr $ reduceValue scopes' x
                -- "App a' b'" is not a redex
                _ ->
                    let after = App (extract a') (extract b') l
                    in  Congruence [a', b'] expr (Value after)
    Lam arg body l ->
        let scopes' = Map.singleton (nameToText arg) NoBinding : scopes
            body'   = reduce scopes' (Value body)
        in  Congruence [body'] expr (Value $ Lam arg (extract body') l)

    Quant op binders range x l ->
        let bindersScope = Map.fromList
                $ map (\binder -> (nameToText binder, NoBinding)) binders
            scopes' = bindersScope : scopes
            op'     = reduce scopes' (Value op)
            range'  = reduce scopes' (Value range)
            x'      = reduce scopes' (Value x)
        in  Congruence
                [op', range', x']
                expr
                (Value $ Quant (extract op')
                               binders
                               (extract range')
                               (extract x')
                               l
                )
    Subst{} -> error "Subst in reduceValue"

    ArrIdx array index l ->
        let array' = reduce scopes (Value array)
            index' = reduce scopes (Value index)
        in  Congruence [array', index']
                       expr
                       (Value $ ArrIdx (extract array') (extract index') l)

    ArrUpd array index value l ->
        let array' = reduce scopes (Value array)
            index' = reduce scopes (Value index)
            value' = reduce scopes (Value value)
        in  Congruence
                [array', index', value']
                expr
                (Value $ ArrUpd (extract array')
                                (extract index')
                                (extract value')
                                l
                )

    others -> Value others

------------------------------------------------------------------

data Binding
  = UserDefinedBinding Expr
  | NoBinding
  | SubstitutionBinding Reason
  deriving (Show)

instance Pretty (Map Text Binding) where
    pretty = pretty . Map.toList

instance Pretty Binding where
    pretty (UserDefinedBinding expr) = "UserDefinedBinding" <+> pretty expr
    pretty (SubstitutionBinding reason) =
        "SubstitutionBinding" <+> pretty reason
    pretty NoBinding = "NoBinding"

instance Pretty Reason where
    pretty (Value val) = "Value" <+> pretty val
    pretty (ExpandContinue name reason) =
        pretty name <+> "=Continue=>" <+> pretty reason
    pretty (ExpandPause _ before after) =
        pretty before <+> "=Pause=>" <+> pretty after
    pretty (ExpandStuck name) = "Stuck" <+> pretty name
    pretty (Congruence _ before after) =
        pretty before <+> "=Cong=>" <+> pretty after

-- pretty (ExpandStuck name) = "Stuck" <+> pretty name
------------------------------------------------------------------

-- | A "Scope" is a mapping from names to Bindings 
type Scope = Map Text Binding

-- | See if a name is in a series of scopes (from local to global)
-- | Return the first result (which should be the most local target)
lookupScopes :: [Scope] -> Name -> Maybe Binding
lookupScopes scopes name = foldl findFirst Nothing scopes
  where
    findFirst :: Maybe Binding -> Scope -> Maybe Binding
    findFirst (Just found) _     = Just found
    findFirst Nothing      scope = Map.lookup (nameToText name) scope

scopeFromLetBindings :: Map Name (Maybe Bindings) -> Scope
scopeFromLetBindings = Map.mapKeys nameToText . fmap toBinding
  where
    toBinding Nothing               = NoBinding
    toBinding (Just (LetBinding x)) = UserDefinedBinding x
    toBinding (Just others) =
        SubstitutionBinding (Value $ bindingsToExpr others)

scopeFromSubstitution :: [Name] -> [Expr] -> Scope
scopeFromSubstitution xs es = Map.mapKeys nameToText $ Map.fromList $ zip
    xs
    (map (SubstitutionBinding . Value) es)
