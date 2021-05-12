{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}

module Render.Class
  ( Render (..),
    RenderBlock(..)
  )
where

import Render.Element
import Data.Text.Prettyprint.Doc (Doc)
import qualified Pretty.Util as Doc
import qualified Data.Text as Text
import Data.Text (Text)

--------------------------------------------------------------------------------

-- | Typeclass for rendering Inline Elements
class Render a where
  render :: a -> Inlines
  renderPrec :: Int -> a -> Inlines

  render = renderPrec 0
  renderPrec = const render

-- | Typeclass for rendering Block Elements
class RenderBlock a where
  renderBlock :: a -> Block
  renderBlockPrec :: Int -> a -> Block

  renderBlock = renderBlockPrec 0
  renderBlockPrec = const renderBlock

-- | Simply "pure . render"
-- renderM :: (Applicative m, Render a) => a -> m Inlines
-- renderM = pure . render

-- | Render instances of Pretty
-- renderP :: (Applicative m, Doc.Pretty a) => a -> m Inlines
-- renderP = pure . text . Doc.renderStrict . Doc.pretty

--------------------------------------------------------------------------------

-- | Other instances of Render
instance Render String where
  render = textE . Text.pack

instance Render Text where
  render = textE

instance Render Int where
  render = render . show

instance Render Integer where
  render = render . show

instance Render Bool where
  render = render . show

instance Render (Doc ann) where
  render = textE . Doc.renderStrict

-- instance Render a => Render [a] where
--   render xs = "[" <> Inlines $ pure $ Horz (punctuate "," (map render xs)) <> "]"
