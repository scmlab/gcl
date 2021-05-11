module Render.WP where 

import Render.Class
import Render.Element
import GCL.WP

instance Render StructWarning where 
  render (MissingBound _) = text "Bound missing at the end of the assertion before the DO construct \" , bnd : ... }\"" 
  render (ExcessBound _) = text "The bound annotation at this assertion is unnecessary"

instance RenderBlock StructWarning where 
  renderBlock x = case x of 
    MissingBound range -> Unlabeled (render x) (Just "Bound Missing") (Just range)
    ExcessBound range -> Unlabeled (render x) (Just "Excess Bound") (Just range)