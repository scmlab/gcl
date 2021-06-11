{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
module Server.Stab2
  ( StabM(..)
  , stabRanged
  , stabLocated
  ) where

import           Control.Monad.Reader           ( MonadReader(ask) )
import           Data.Loc                       ( Located(locOf) )
import           Data.Loc.Range
import qualified Language.LSP.Types            as J
import qualified Server.Util                   as J

stabbed :: Ranged a => J.Position -> a -> Bool
stabbed position node =
  let Range start end = rangeOf node
  in  J.toPos start `cmp` position /= GT && position `cmp` J.toPos end /= GT
 where
  cmp :: J.Position -> J.Position -> Ordering
  cmp (J.Position lineA colA) (J.Position lineB colB) =
    case lineA `compare` lineB of
      LT -> LT
      EQ -> colA `compare` colB
      GT -> GT

stabbed' :: Located a => J.Position -> a -> Bool
stabbed' position node = case fromLoc (locOf node) of
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

-- | Like `Stab` but in some context
class StabM m a b where
  stabM :: (Monad m, MonadReader J.Position m, StabM m a b) => a -> m [b]

stabRanged
  :: (Monad m, Ranged a, MonadReader J.Position m, StabM m a b) => a -> m [b]
stabRanged node = do
  pos <- ask
  if pos `stabbed` rangeOf node then stabM node else return []

stabLocated
  :: (Monad m, Located a, MonadReader J.Position m, StabM m a b) => a -> m [b]
stabLocated node = do
  pos <- ask
  if pos `stabbed'` locOf node then stabM node else return []

instance (Monad m, StabM m a b) => StabM m (Maybe a) b where
  stabM Nothing  = return []
  stabM (Just x) = stabM x

instance (Monad m, StabM m a b) => StabM m [a] b where
  stabM xs = concat <$> mapM stabM xs
