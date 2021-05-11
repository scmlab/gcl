{-# LANGUAGE OverloadedStrings #-}

module Render.Class
  ( Render (..),
    -- RenderTCM (..),
    -- renderM,
    -- renderP,
    -- renderA,
    -- renderATop,
  )
where

import Render.Element

--------------------------------------------------------------------------------

-- | Typeclass for rendering Inline Element
class Render a where
  render :: a -> Element
  renderPrec :: Int -> a -> Element

  render = renderPrec 0
  renderPrec = const render

-- | Simply "pure . render"
-- renderM :: (Applicative m, Render a) => a -> m Inlines
-- renderM = pure . render

-- | Render instances of Pretty
-- renderP :: (Applicative m, Doc.Pretty a) => a -> m Inlines
-- renderP = pure . text . Doc.renderStrict . Doc.pretty

--------------------------------------------------------------------------------

-- | Other instances of Render
instance Render Int where
  render = text . show

instance Render Integer where
  render = text . show

instance Render Bool where
  render = text . show

-- instance Render Doc where
--   render = text . Doc.renderStrict . Doc.pretty

-- instance Render a => Render [a] where
--   render xs = "[" <> Inlines $ pure $ Horz (punctuate "," (map render xs)) <> "]"
