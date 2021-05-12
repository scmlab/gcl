module Render.Syntax.Common where 

import Render.Class
import Render.Element
import Syntax.Common
import Data.Loc.Range

instance Render Name where 
  render (Name t loc) = case fromLoc loc of 
    Nothing -> render t 
    Just range -> linkE range $ render t 
-- -- | Variables and stuff
-- data Name = Name Text Loc
--   deriving (Show, Generic)
