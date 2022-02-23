{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleContexts #-}

module Server.IntervalMap
  ( IntervalMap
  , singleton
  , toList
  , fromList
  , insert
  , lookup
  , split
  , Scope
  , M
  , runM
  , Collect(..)
  , lookupScopes
  , localScope
  ) where

import           Control.Monad.RWS
import           Data.Bifunctor                 ( bimap )
import qualified Data.Foldable                 as Foldable
import           Data.IntMap                    ( IntMap )
import qualified Data.IntMap                   as IntMap
import           Data.List.NonEmpty             ( NonEmpty )
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
import           Prettyprinter
import           Prelude                 hiding ( lookup )
import           Syntax.Concrete                ( SepBy )


--------------------------------------------------------------------------------
-- Uses IntMap internally for speeding up lookups
-- with the key of IntMap acting as the starting offset,
-- and the element's Int acting as the ending offset
newtype IntervalMap token = IntervalMap (IntMap (Int, token)) deriving (Eq, Monoid, Semigroup)

instance Functor IntervalMap where
  fmap f (IntervalMap m) = IntervalMap (IntMap.map (fmap f) m)

-- Instances for debugging
instance Pretty token => Show (IntervalMap token) where
  show = show . pretty

instance Pretty token => Pretty (IntervalMap token) where
  pretty (IntervalMap xs) =
    vcat
      $ Prelude.map
          (\(start, (end, token)) ->
            "(" <> pretty start <> ", " <> pretty end <> ") => " <> pretty token
          )
      $ IntMap.toList xs

instance Foldable IntervalMap where
  foldMap f (IntervalMap xs) = foldMap (f . snd) xs

--------------------------------------------------------------------------------
-- Construction 

-- Constructs a IntervalMap with a Range and a payload
singleton :: Range -> token -> IntervalMap token
singleton range token = IntervalMap $ IntMap.singleton
  (posCoff (rangeStart range))
  (posCoff (rangeEnd range), token)

toList :: IntervalMap token -> [((Int, Int), token)]
toList (IntervalMap m) = map (\(a, (b, c)) -> ((a, b), c)) (IntMap.toList m)

fromList :: [((Int, Int), token)] -> IntervalMap token
fromList = IntervalMap . IntMap.fromList . map (\((a, b), c) -> (a, (b, c)))

--------------------------------------------------------------------------------
-- Insertion  

insert :: Range -> token -> IntervalMap token -> IntervalMap token
insert range token (IntervalMap m) = IntervalMap $ IntMap.insert
  (posCoff (rangeStart range))
  (posCoff (rangeEnd range), token)
  m

split :: Range -> IntervalMap token -> (IntervalMap token, IntervalMap token)
split rng (IntervalMap m) =
  bimap IntervalMap IntervalMap (IntMap.split (posCoff (rangeStart rng)) m)

--------------------------------------------------------------------------------
-- Query 

-- Given a Pos, returns the paylod and its Range if the Pos is within its Range
lookup' :: Pos -> IntervalMap token -> Maybe ((Int, Int), token)
lookup' pos (IntervalMap m) =
  let offset = posCoff pos
  in  case IntMap.lookupLE offset m of
        Nothing -> Nothing
        Just (start, (end, x)) ->
          if offset <= end then Just ((start, end), x) else Nothing

-- Given a Pos, returns the paylod if the Pos is within its Range
lookup :: Pos -> IntervalMap token -> Maybe token
lookup pos m = snd <$> lookup' pos m

--------------------------------------------------------------------------------

-- | A mapping from names to something else
type Scope input = Map Text input

-- | Accumulates the result of `IntervalMap` in writer
--   Stores stack of scopes in reader
type M input output = RWS [Scope input] (IntervalMap output) ()

runM :: [Scope input] -> M input output a -> IntervalMap output
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

instance Collect input output a => Collect input output (NonEmpty a) where
  collect = mapM_ collect

instance Collect input output a => Collect input output (Map k a) where
  collect = mapM_ collect

instance (Collect input output a, Collect input output b) => Collect input output (Either a b) where
  collect (Left  a) = collect a
  collect (Right a) = collect a

instance Collect input output a => Collect input output (SepBy tok a) where
  collect xs = forM_ (Foldable.toList xs) collect
