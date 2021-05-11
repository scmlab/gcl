module Render.WP where 

import Render.Class
import Render.Element
import GCL.WP

instance Render StructWarning where 
  render (MissingBound loc) = text "Bound missing at the end of the assertion before the DO construct \" , bnd : ... }\"" 
  render (ExcessBound loc) = text "The bound annotation at this assertion is unnecessary"