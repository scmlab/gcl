module Pretty.Common where

import           Prelude                 hiding ( Ordering(..) )
import           Pretty.Util
import           Prettyprinter                  ( Pretty(pretty) )
import           Syntax.Common

import           Render.Error                   ( )
import           Render.Predicate               ( )
import           Render.Syntax.Common           ( )

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

instance Pretty TypeOp where
  pretty = toDoc . prettyWithLoc

instance PrettyWithLoc TypeOp where
  prettyWithLoc = fromRenderAndLocated

instance Pretty Op where
  pretty = toDoc . prettyWithLoc

instance PrettyWithLoc Op where
  prettyWithLoc = fromRenderAndLocated
