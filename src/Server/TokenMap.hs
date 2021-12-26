{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleContexts #-}

module Server.TokenMap
  ( TokenMap
  , singleton
  , Scope
  , M
  , runM
  , Collect(..)
  , insert
  , lookup
  , lookupRng
  , lookupScopes
  , localScope
  , split
  , toList
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
import           Data.Bifunctor                 ( bimap )
import           Prelude                 hiding ( lookup )
import           Data.Text.Prettyprint.Doc


--------------------------------------------------------------------------------
-- IntMap for speeding up lookups
-- with the key of IntMap acting as the starting offset,
-- and the element's Int acting as the ending offset
newtype TokenMap token = TokenMap (IntMap (Int, token)) deriving (Eq, Monoid, Semigroup)

instance Functor TokenMap where
  fmap f (TokenMap m) = TokenMap (IntMap.map (fmap f) m)

-- Instances for debugging
instance Pretty token => Show (TokenMap token) where
  show = show . pretty

instance Pretty token => Pretty (TokenMap token) where
  pretty (TokenMap xs) =
    vcat
      $ Prelude.map
          (\(start, (end, token)) ->
            "(" <> pretty start <> ", " <> pretty end <> ") => " <> pretty token
          )
      $ IntMap.toList xs

-- Constructs a TokenMap with a Range and a payload
singleton :: Range -> token -> TokenMap token
singleton range token = TokenMap $ IntMap.singleton
  (posCoff (rangeStart range))
  (posCoff (rangeEnd range), token)

insert :: Range -> token -> TokenMap token -> TokenMap token
insert range token (TokenMap m) = TokenMap $ IntMap.insert
  (posCoff (rangeStart range))
  (posCoff (rangeEnd range), token)
  m

-- Given a Pos, returns the paylod if the Pos is within its Range
lookup :: TokenMap token -> Pos -> Maybe token
lookup (TokenMap m) pos =
  let offset = posCoff pos
  in  case IntMap.lookupLE offset m of
        Nothing                 -> Nothing
        Just (_start, (end, x)) -> if offset <= end then Just x else Nothing

lookupRng :: TokenMap token -> Range -> Maybe token
lookupRng (TokenMap m) rng =
  let offsetStart = posCoff (rangeStart rng)
  in  let offsetEnd = posCoff (rangeEnd rng)
      in  IntMap.lookupLE offsetStart m
            >>= \(_, (end, x)) -> if offsetEnd <= end then Just x else Nothing

split :: Range -> TokenMap token -> (TokenMap token, TokenMap token)
split rng (TokenMap m) =
  bimap TokenMap TokenMap (IntMap.split (posCoff (rangeStart rng)) m)

toList :: TokenMap token -> [((Int, Int), token)]
toList (TokenMap m) = map (\(a, (b, c)) -> ((a, b), c)) (IntMap.toList m)

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

localScope :: MonadReader [Scope input] m => Scope input -> m a -> m a
localScope scope = local (scope :)

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
