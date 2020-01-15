{-# LANGUAGE DeriveFunctor, OverloadedStrings #-}

module Pretty.Concrete where

import Data.Text.Prettyprint.Doc
-- import Prelude hiding (Ordering(..))

import Syntax.Location
import Syntax.Concrete
import Pretty.Abstract ()

--------------------------------------------------------------------------------
-- | Expr

instance Pretty Expr where
  pretty = pretty . depart

--------------------------------------------------------------------------------
-- | Type

-- instance Pretty Endpoint where
--   pretty (Including e) = ""
instance Pretty Interval where
  pretty = pretty . depart

instance Pretty Type where
  pretty = pretty . depart
