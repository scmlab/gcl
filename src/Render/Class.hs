{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}

module Render.Class
  ( Render (..),
    RenderSection(..), 
    tempHandleLoc,
    renderManySepByComma
  )
where

import Render.Element
import qualified Data.Text as Text
import Data.Text (Text)
import Data.Loc.Range (fromLoc)
import Data.Loc (Loc)
import Data.Text.Prettyprint.Doc (Doc)
import qualified Data.Text.Prettyprint.Doc.Render.Text as Text
import qualified Data.Text.Prettyprint.Doc as Doc

--------------------------------------------------------------------------------

-- | Typeclass for rendering Inline Elements
class Render a where
  render :: a -> Inlines
  renderPrec :: Int -> a -> Inlines

  render = renderPrec 0
  renderPrec = const render

-- | Typeclass for rendering Sections
class RenderSection a where
  renderSection :: a -> Section
  renderSectionPrec :: Int -> a -> Section

  renderSection = renderSectionPrec 0
  renderSectionPrec = const renderSection

--------------------------------------------------------------------------------

tempHandleLoc :: Loc -> Inlines -> Inlines 
tempHandleLoc loc t = case fromLoc loc of 
  Nothing -> t
  Just range -> linkE range t 

-- renderLocatedAndPrettified :: (Located a, Pretty a) => a -> Inlines 
-- renderLocatedAndPrettified x = tempHandleLoc (locOf x) (render $ pretty x) 

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
  render = textE . Text.renderStrict . Doc.layoutPretty Doc.defaultLayoutOptions

instance (Render a) => Render (Maybe a) where
  render Nothing = "Nothing"
  render (Just a) = "Just" <+> render a

instance (Render a, Render b) => Render (a, b) where
  render (a, b) = "(" <+> render a <+> "," <+> render b <+> ")"

-- seperated by commas
-- instance Render a => Render [a] where
--   render xs = punctuateE "," . map render
--       "[" <> Inlines $ pure $ Horz (punctuate "," (map render xs)) <> "]"

renderManySepByComma :: Render a => [a] -> Inlines
renderManySepByComma = punctuateE "," . map render