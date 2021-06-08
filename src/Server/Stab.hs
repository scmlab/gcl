{-# LANGUAGE MultiParamTypeClasses #-}
module Server.Stab
  ( Stab (..),
    stabMaybe,
    StabM (..),
    stabMaybeM,
    stabbed,
    Collect (..),
  )
where

import Data.Loc.Range
import qualified Language.LSP.Types as J
import qualified Server.Util as J

--------------------------------------------------------------------------------

-- | O(n), should improve the time complexity with some segment tree
class Stab a b where
  stab :: J.Position -> a -> [b]

stabMaybe :: Stab a b => J.Position -> a -> Maybe b
stabMaybe pos node = case stab pos node of
  [] -> Nothing
  (x : _) -> Just x

stabbed :: Ranged a => J.Position -> a -> Bool
stabbed position node =
  let Range start end = rangeOf node
   in J.toPos start `cmp` position /= GT
        && position `cmp` J.toPos end /= GT
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
  stabM :: J.Position -> a -> m [b]

stabMaybeM :: (Monad m, StabM m a b) => J.Position -> a -> m (Maybe b)
stabMaybeM pos node = do
  result <- stabM pos node
  case result of
    [] -> return Nothing
    (x : _) -> return (Just x)

--------------------------------------------------------------------------------

class Collect a b where
  collect :: a -> [b]
