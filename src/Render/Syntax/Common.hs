module Render.Syntax.Common where 

import Render.Class
import Syntax.Common

instance Render Name where 
  render = renderLocatedAndPrettified

instance Render ChainOp where
  render = renderLocatedAndPrettified

instance Render ArithOp where
  render = renderLocatedAndPrettified

instance Render QuantOp where
  render = renderLocatedAndPrettified

instance Render Op where
  render = renderLocatedAndPrettified
