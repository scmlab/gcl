module Pretty.Common where

import Prettyprinter (Pretty (pretty))
import Pretty.Util
import Syntax.Common
import Prelude hiding (Ordering (..))


-- | Name
instance Pretty Name where
  pretty = toDoc . prettyWithLoc
  
instance PrettyWithLoc Name where
  prettyWithLoc = fromRenderAndLocated

-- | Operators
instance Pretty ChainOp where
  pretty = toDoc . prettyWithLoc

instance PrettyWithLoc ChainOp where
  prettyWithLoc = fromRenderAndLocated

instance Pretty ArithOp where
  pretty = toDoc . prettyWithLoc

instance PrettyWithLoc ArithOp where
  prettyWithLoc = fromRenderAndLocated

instance Pretty Op where
  pretty = toDoc . prettyWithLoc

instance PrettyWithLoc Op where
  prettyWithLoc = fromRenderAndLocated
