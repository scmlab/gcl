{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}

module Pretty
  ( module Data.Text.Prettyprint.Doc,
    module Pretty.Util 
  )
where

import Data.Text.Prettyprint.Doc
import Error (Error)
import Pretty.Abstract ()
import Pretty.Concrete ()
import Pretty.Error ()
import Pretty.Util
import Pretty.Predicate ()
import Prelude hiding (Ordering (..))

--------------------------------------------------------------------------------

-- | Misc
instance {-# OVERLAPPING #-} (Pretty a) => Pretty (Either Error a) where
  pretty (Left a) = "Error" <+> pretty a
  pretty (Right b) = pretty b

instance (Pretty a, Pretty b, Pretty c, Pretty d) => Pretty (a, b, c, d) where
  pretty (a, b, c, d) = "(" <> pretty a <> ", " <> pretty b <> ", " <> pretty c <> ", " <> pretty d <> ")"
