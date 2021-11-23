{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Server.TokenMap
  ( TokenMap
  , singleton
  , Scope
  , M
  , runM
  , Collect(..)
  , lookup
  , lookupScopes
  , localScope
  ) where

import           Control.Monad.RWS
import           Data.IntMap                    ( IntMap )
import qualified Data.IntMap                   as IntMap
import           Data.Loc                       ( Pos
                                                , posCoff
                                                )
import           Data.Loc.Range                 ( Range
                                                , rangeEnd
                                                , rangeStart
                                                )
import           Data.Map                       ( Map )
import qualified Data.Map                      as Map
import           Data.Text                      ( Text )
import           Prelude                 hiding ( lookup )
import           Pretty


--------------------------------------------------------------------------------
-- IntMap for speeding up lookups 
-- with the key of IntMap acting as the starting offset, 
-- and the element's Int acting as the ending offset 
newtype TokenMap token = TokenMap (IntMap (Int, token)) deriving (Eq, Monoid, Semigroup)

-- Instances for debugging 
instance Pretty token => Show (TokenMap token) where
  show = show . pretty

instance Pretty token => Pretty (TokenMap token) where
  pretty (TokenMap xs) =
    vcat
      $ map
          (\(start, (end, token)) ->
            "(" <> pretty start <> ", " <> pretty end <> ") => " <> pretty token
          )
      $ IntMap.toList xs

-- Constructs a TokenMap with a Range and a payload
singleton :: Range -> token -> TokenMap token
singleton range token = TokenMap $ IntMap.singleton
  (posCoff (rangeStart range))
  (posCoff (rangeEnd range), token)

-- Given a Pos, returns the paylod if the Pos is within its Range 
lookup :: TokenMap token -> Pos -> Maybe token
lookup (TokenMap m) pos =
  let offset = posCoff pos
  in  case IntMap.lookupLE offset m of
        Nothing                 -> Nothing
        Just (_start, (end, x)) -> if offset <= end then Just x else Nothing

--------------------------------------------------------------------------------

-- | A mapping from names to something else
type Scope input = Map Text input

-- | Accumulates the result of `TokenMap` in writer 
--   Stores stack of scopes in reader 
type M input output = RWS [Scope input] (TokenMap output) ()

runM :: [Scope input] -> M input output a -> TokenMap output
runM scopes f = let (_, _, w) = runRWS f scopes () in w

-- | See if a name is in a series of scopes (from local to global)
-- | Return the first result (which should be the most local target)
lookupScopes :: Text -> M input output (Maybe input)
lookupScopes name = asks lookupScopesPrim
 where
  lookupScopesPrim :: [Scope input] -> Maybe input
  lookupScopesPrim scopes = foldl findFirst Nothing scopes

  findFirst :: Maybe input -> Scope input -> Maybe input
  findFirst (Just found) _     = Just found
  findFirst Nothing      scope = Map.lookup name scope

localScope :: Scope input -> M input output a -> M input output a
localScope = pushScope
 where
  pushScope :: Scope input -> M input output a -> M input output a
  pushScope scope = local (scope :)

--------------------------------------------------------------------------------

-- | Given a Abstract syntax node, returns a mapping of Range-Info
class Collect input output a where
  collect :: a -> M input output ()

instance Collect input output a => Collect input output (Maybe a) where
  collect Nothing  = return ()
  collect (Just x) = collect x

instance Collect input output a => Collect input output [a] where
  collect = mapM_ collect

instance Collect input output a => Collect input output (Map k a) where
  collect = mapM_ collect

instance (Collect input output a, Collect input output b) => Collect input output (Either a b) where
  collect (Left  a) = collect a
  collect (Right a) = collect a
