{-# LANGUAGE OverloadedStrings #-}

module Render.Class
  ( Render (..),
    RenderBlock(..)
    -- RenderTCM (..),
    -- renderM,
    -- renderP,
    -- renderA,
    -- renderATop,
  )
where

import Render.Element

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
