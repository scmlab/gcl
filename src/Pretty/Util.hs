module Pretty.Util where

import           Data.Text.Prettyprint.Doc
import           Prelude                 hiding ( Ordering(..) )
import           Data.Loc

instance Pretty Loc where
  pretty = pretty . displayLoc

--------------------------------------------------------------------------------
-- | Pretty print with Precedence

class PrettyPrec a where
  prettyPrec :: Int -> a -> Doc ann

