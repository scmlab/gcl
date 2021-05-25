{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}

module Pretty
  ( module Data.Text.Prettyprint.Doc,
    toText,
    docToText,
    toString,
    docToString
  )
where

import Data.Text (Text)
import qualified Data.Text as Text
import Data.Text.Prettyprint.Doc
import Data.Text.Prettyprint.Doc.Render.Text (renderStrict)
import Error (Error)
import Pretty.Abstract ()
import Pretty.Concrete ()
import Pretty.Error ()
import Pretty.Predicate ()
import Prelude hiding (Ordering (..))

docToText :: Doc ann -> Text
docToText = renderStrict . layoutCompact

toText :: Pretty a => a -> Text
toText = docToText . pretty

docToString :: Doc ann -> String
docToString = Text.unpack . docToText

toString :: Pretty a => a -> String
toString = Text.unpack . toText

--------------------------------------------------------------------------------

-- | Misc
instance {-# OVERLAPPING #-} (Pretty a) => Pretty (Either Error a) where
  pretty (Left a) = "Error" <+> pretty a
  pretty (Right b) = pretty b

instance (Pretty a, Pretty b, Pretty c, Pretty d) => Pretty (a, b, c, d) where
  pretty (a, b, c, d) = "(" <> pretty a <> ", " <> pretty b <> ", " <> pretty c <> ", " <> pretty d <> ")"
