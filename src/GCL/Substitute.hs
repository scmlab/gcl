module GCL.Substitute where

import           Data.Map                       ( Map )
import qualified Data.Map                      as Map
import           Data.Text                      ( Text )
import           Syntax.Abstract                ( Expr(..) )
import           Syntax.Common                  ( Name
                                                , nameToText
                                                )

------------------------------------------------------------------

-- explains how a value or expression came to be 
data Reason
    = ExpandOthers Name Reason
    | ExpandUserDefined Name Reason
    | Reduce Expr Reason
    | ReduceSub [Reason] Expr
    -- | UserDefined Name Expr
    | Value Expr

-- userDefined :: Name -> Expr -> Derivation
-- userDefined name expr = Derivation [] (UserDefined name expr) 

data Derivation = Derivation [Derivation] Reason


------------------------------------------------------------------

-- data Reduction = Reduction Expr Expr
            --    | 

-- reduce :: Expr -> Reduction
-- reduce expr = Reduction expr (betaReduction expr)

------------------------------------------------------------------

extract :: Reason -> Expr
extract (ExpandOthers      _ x) = extract x
extract (ExpandUserDefined y x) = Expand y (extract x)
extract (Reduce            _ x) = extract x
extract (ReduceSub         _ x) = x
extract (Value x              ) = x

reduce' :: Scopes -> Expr -> (Reason, Expr)
reduce' scopes expr =
    let reason = reduceValue scopes expr in (reason, extract reason)


reduce :: Scopes -> Reason -> Reason
reduce scopes expr = case expr of 
    Value val -> reduceValue scopes val 
    others -> others 

reduceValue :: Scopes -> Expr -> Reason
reduceValue scopes expr = case expr of

    Paren e l ->
        let (eReason, e') = reduce' scopes e
        in  ReduceSub [eReason] $ Paren e' l

    Var name _ -> case lookupScopes scopes name of
        Nothing -> error "panic: not in scope"
        Just (UserDefinedBinding binding) ->
            ExpandUserDefined name (reduce scopes binding)
        Just (OthersBinding binding) -> ExpandOthers name (reduce scopes binding)

    Const name _ -> case lookupScopes scopes name of
        Nothing -> error "panic: not in scope"
        Just (UserDefinedBinding binding) ->
            ExpandUserDefined name (reduce scopes binding)
        Just (OthersBinding binding) -> ExpandOthers name (reduce scopes binding)

    Chain a op b l ->
        let (aReason, a') = reduce' scopes a
            (bReason, b') = reduce' scopes b
        in  ReduceSub [aReason, bReason] $ Chain a' op b' l

    App a b l -> 
        -- reduce "a" all the way down 
        let (aReason, a') = reduce' scopes a
            (bReason, b') = reduce' scopes b
        in case a' of 
            -- "App a' b'" is a redex 
            Lam n x _ -> 
                -- reduce "x" all the way down 
                reduceValue (Map.singleton (nameToText n) (OthersBinding bReason) : scopes) x
            -- "App a' b'" is not a redex 
            _ -> ReduceSub [aReason, bReason] $ App a' b' l
    -- App (Lam n x _) b l ->
    --     let (bReason, b') = reduce' scopes b
    --         (aReason, a') =
    --             reduce' (Map.singleton (nameToText n) (OthersBinding bReason) : scopes) x

    --     in  ReduceSub [aReason, bReason] $ App a' b' l

    -- -- is not a redex 
    -- App a b l ->
    --     let (aReason, a') = reduce' scopes a
    --         (bReason, b') = reduce' scopes b
    --     in  ReduceSub [aReason, bReason] $ App a' b' l

    others -> Value others



------------------------------------------------------------------

type Scopes = [Scope Binding]

data Binding = UserDefinedBinding Reason | OthersBinding Reason

------------------------------------------------------------------

-- | A "Scope" is a mapping from names to values
type Scope a = Map Text a

-- | See if a name is in a series of scopes (from local to global)
-- | Return the first result (which should be the most local target)
lookupScopes :: [Scope a] -> Name -> Maybe a
lookupScopes scopes name = foldl findFirst Nothing scopes
  where
    findFirst :: Maybe a -> Scope a -> Maybe a
    findFirst (Just found) _     = Just found
    findFirst Nothing      scope = Map.lookup (nameToText name) scope

