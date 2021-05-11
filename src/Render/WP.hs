module Render.WP where 

import Render.Class
import Render.Element
import GCL.WP

instance Render StructWarning where 
  render (MissingBound _) = text "Bound missing at the end of the assertion before the DO construct \" , bnd : ... }\"" 
  render (ExcessBound _) = text "The bound annotation at this assertion is unnecessary"

instance RenderBlock StructWarning where 
  renderBlock x = case x of 
    MissingBound _loc -> Unlabeled (render x) (Just "Bound Missing") Nothing
    ExcessBound _loc -> Unlabeled (render x) (Just "Excess Bound") Nothing