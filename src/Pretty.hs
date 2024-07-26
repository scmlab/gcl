{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}

module Pretty
  ( module Prettyprinter
  , module Pretty.Util
  ) where

import           Error                          ( Error )
import           Prelude                 hiding ( Ordering(..) )
import           Pretty.Abstract                ( )
import           Pretty.Concrete                ( )
import           Pretty.Error                   ( )
import           Pretty.Predicate               ( )
import           Pretty.Typed                   ( )
import           Pretty.Util
import           Prettyprinter

--------------------------------------------------------------------------------

-- | Misc
instance {-# OVERLAPPING #-} (Pretty a) => Pretty (Either Error a) where
  pretty (Left  a) = "Error" <+> pretty a
  pretty (Right b) = pretty b

instance (Pretty a, Pretty b, Pretty c, Pretty d) => Pretty (a, b, c, d) where
  pretty (a, b, c, d) =
    "("
      <> pretty a
      <> ", "
      <> pretty b
      <> ", "
      <> pretty c
      <> ", "
      <> pretty d
      <> ")"
