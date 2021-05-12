-- {-# LANGUAGE OverloadedStrings #-}

module Render.Error where 

-- import Render.Class
-- import Render.Element
-- import Error

-- instance RenderBlock Error where 
--   renderBlock (SyntacticError xs) = 
-- = SyntacticError [SyntacticError]
-- | TypeError TypeError
-- | StructError StructError
-- | CannotReadFile FilePath
-- | Others String = undefined
--   render (MissingBound _) = "Bound missing at the end of the assertion before the DO construct \" , bnd : ... }\"" 
--   render (ExcessBound _) = "The bound annotation at this assertion is unnecessary"

-- instance RenderBlock StructWarning where 
--   renderBlock x = case x of 
--     MissingBound range -> blockE (Just "Bound Missing") (Just range) (render x)
--     ExcessBound range -> blockE (Just "Excess Bound") (Just range) (render x)