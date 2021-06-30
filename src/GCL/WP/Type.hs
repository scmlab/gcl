{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}

module GCL.WP.Type where

import Data.Aeson (ToJSON)
import Data.Loc (Loc (..), Located (..))
import Data.Loc.Range (Range)
import GCL.Common ()
import GHC.Generics (Generic)

data StructWarning
  = MissingBound Range
  | ExcessBound Range
  deriving (Eq, Show, Generic)

instance Located StructWarning where
  locOf (MissingBound rng) = locOf rng
  locOf (ExcessBound rng) = locOf rng

data StructError
  = MissingAssertion Loc
  | MissingPostcondition Loc
  | MultiDimArrayAsgnNotImp Loc
     -- Assignment to multi-dimensional array not implemented.
     -- SCM: will remove this when we figure out how.
  deriving (Eq, Show, Generic)

instance Located StructError where
  locOf (MissingAssertion l) = l
  locOf (MissingPostcondition l) = l
  locOf (MultiDimArrayAsgnNotImp l) = l

instance ToJSON StructError
