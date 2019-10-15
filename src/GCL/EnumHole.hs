module GCL.EnumHole where

import Control.Monad.State
import qualified Data.Map as Map

--------------------------------------------------------------------------------
-- | Enumerate and index Holes

type HoleIndex = Int
type EnumHoleM = State HoleIndex

class EnumHole p where
  enumHole :: p -> EnumHoleM p

runEnumHole :: EnumHole p => p -> p
runEnumHole x = evalState (enumHole x) 0

freshHole :: EnumHoleM HoleIndex
freshHole = do
  i <- get
  put (succ i)
  return i
