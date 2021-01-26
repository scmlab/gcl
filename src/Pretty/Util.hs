{-# LANGUAGE OverloadedStrings #-}

module Pretty.Util
  ( renderStrict,
    PrettyPrec (..),
    PrettyWithLoc (..),
    DocWithLoc (..),
    toDoc,
    fromDoc,
  )
where

import Data.Loc
import Data.Text (Text)
import Data.Text.Prettyprint.Doc
import qualified Data.Text.Prettyprint.Doc.Render.Text as Text
import Prelude hiding (Ordering (..))

instance Pretty Loc where
  pretty = pretty . displayLoc

renderStrict :: Doc ann -> Text
renderStrict = Text.renderStrict . layoutPretty defaultLayoutOptions

--------------------------------------------------------------------------------

-- | Prettifier that respects Locs
data DocWithLoc ann
  = -- | A piece of Doc with starting and ending Position
    DocWithLoc (Doc ann) Pos Pos
  | -- | As `mempty`
    Empty
  deriving (Show)

-- | Appends two DocWithLoc in a srcloc-respecting way
append :: DocWithLoc ann -> DocWithLoc ann -> DocWithLoc ann
append Empty Empty = Empty
append Empty (DocWithLoc y c d) = DocWithLoc y c d
append (DocWithLoc x a b) Empty = DocWithLoc x a b
append (DocWithLoc x a b) (DocWithLoc y c d) =
  if c >= b
    then DocWithLoc (x <> fillGap b c <> y) a d
    else DocWithLoc (y <> fillGap c b <> x) c b

instance Semigroup (DocWithLoc ann) where
  (<>) = append

instance Monoid (DocWithLoc ann) where
  mappend = (<>)
  mempty = Empty

fromDoc :: Loc -> Doc ann -> DocWithLoc ann
fromDoc NoLoc _ = Empty
fromDoc (Loc a b) x = DocWithLoc x a b

toDoc :: DocWithLoc ann -> Doc ann
toDoc (DocWithLoc d a _) = fillGap (Pos (posFile a) 1 0 0) a <> d
toDoc Empty = mempty 

-- generates newlines and spaces to fill the gap between to Pos
fillGap :: Pos -> Pos -> Doc ann
fillGap this next =
  let lineDiff = posLine next - posLine this
   in if lineDiff == 0
        then -- on the same line, just pad them with spaces

          let colDiff = posCol next - 1 - posCol this
           in mconcat (replicate colDiff space)
        else -- on different lines
          mconcat (replicate lineDiff "\n" ++ replicate (posCol next - 1) space)

--------------------------------------------------------------------------------

-- | Pretty print with Precedence
class PrettyPrec a where
  prettyPrec :: Int -> a -> Doc ann

class PrettyWithLoc a where
  prettyWithLoc :: a -> DocWithLoc ann
