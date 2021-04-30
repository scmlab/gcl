{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}

module Pretty
  ( module Data.Text.Prettyprint.Doc,
    renderStrict,
    -- renderLazy,
  )
where

import Data.Text (Text)
import Data.Text.Prettyprint.Doc
import qualified Data.Text.Prettyprint.Doc.Render.Text as Text
import Pretty.Error ()
import Pretty.Abstract ()
import Pretty.Concrete ()
import Pretty.Predicate ()
import Prelude hiding (Ordering (..))

renderStrict :: Doc ann -> Text
renderStrict = Text.renderStrict . layoutPretty defaultLayoutOptions

-- renderLazy :: Doc ann -> Lazy.Text
-- renderLazy = Text.renderLazy . layoutPretty defaultLayoutOptions

--------------------------------------------------------------------------------

-- | Misc
instance (Pretty a, Pretty b) => Pretty (Either a b) where
  pretty (Left a) = "Error" <+> pretty a
  pretty (Right b) = pretty b

--------------------------------------------------------------------------------

--------------------------------------------------------------------------------

-- --------------------------------------------------------------------------------
-- -- | Val
--
-- instance Pretty Val where
--   pretty = pretty . show
