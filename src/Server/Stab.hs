{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
module Server.Stab
  ( StabM(..)
  , stabRanged
  , stabLocated
  , HasPosition(..)
  , Scope
  , HasScopes(..)
  , lookupScopes
  , Collect(..)
  ) where

import           Data.Loc                       ( Located(locOf) )
import           Data.Loc.Range
import           Data.Map                       ( Map )
import qualified Data.Map                      as Map
import           Data.Text                      ( Text )
import qualified Language.LSP.Types            as J
import qualified Server.Util                   as J

--------------------------------------------------------------------------------

-- | Like `Stab` but in some context
class StabM m a b where
  stabM :: (Monad m, HasPosition m, StabM m a b) => a -> m [b]

stabRanged :: (Monad m, Ranged a, HasPosition m, StabM m a b) => a -> m [b]
stabRanged node = do
  pos <- askPosition
  if pos `stabbedRanged` rangeOf node then stabM node else return []

stabLocated :: (Monad m, Located a, HasPosition m, StabM m a b) => a -> m [b]
stabLocated node = do
  pos <- askPosition
  if pos `stabbedLocated` locOf node then stabM node else return []

instance (Monad m, StabM m a b) => StabM m (Maybe a) b where
  stabM Nothing  = return []
  stabM (Just x) = stabM x

instance (Monad m, StabM m a b) => StabM m [a] b where
  stabM xs = concat <$> mapM stabM xs


--------------------------------------------------------------------------------
-- HasPosition

class HasPosition m where
  askPosition :: m J.Position

--------------------------------------------------------------------------------
-- HasScopes

class HasScopes m a where
  askScopes :: m [Scope a]
  -- temporarily prepend a local scope to the scope list 
  localScope :: Scope a -> m b -> m b

lookupScopes :: (HasScopes m a, Monad m) => Text -> m (Maybe a)
lookupScopes name = do
  scopes <- askScopes
  return $ lookupScopesPrim scopes name

-- | A "Scope" is a mapping of names and LocationLinks
type Scope a = Map Text a

-- | See if a name is in a series of scopes (from local to global)
-- | Return the first result (which should be the most local target)
lookupScopesPrim :: [Scope a] -> Text -> Maybe a
lookupScopesPrim scopes name = foldl findFirst Nothing scopes
 where
  findFirst :: Maybe a -> Scope a -> Maybe a
  findFirst (Just found) _     = Just found
  findFirst Nothing      scope = Map.lookup name scope


--------------------------------------------------------------------------------
-- helper functions 

stabbedRanged :: Ranged a => J.Position -> a -> Bool
stabbedRanged position node =
  let Range start end = rangeOf node
  in  J.toPos start `cmp` position /= GT && position `cmp` J.toPos end /= GT
 where
  cmp :: J.Position -> J.Position -> Ordering
  cmp (J.Position lineA colA) (J.Position lineB colB) =
    case lineA `compare` lineB of
      LT -> LT
      EQ -> colA `compare` colB
      GT -> GT

stabbedLocated :: Located a => J.Position -> a -> Bool
stabbedLocated position node = case fromLoc (locOf node) of
  Nothing -> False
  Just (Range start end) ->
    J.toPos start `cmp` position /= GT && position `cmp` J.toPos end /= GT
 where
  cmp :: J.Position -> J.Position -> Ordering
  cmp (J.Position lineA colA) (J.Position lineB colB) =
    case lineA `compare` lineB of
      LT -> LT
      EQ -> colA `compare` colB
      GT -> GT

--------------------------------------------------------------------------------

class Collect a b where
  collect :: a -> [b]
