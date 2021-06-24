{-# LANGUAGE OverloadedStrings #-}
module Pretty.Util
  ( 
    docToText, toText,
    docToString, toString,
    PrettyPrec (..),
    PrettyWithLoc (..),
    DocWithLoc (..),
    toDoc,
    fromDoc,
    fromRender,
    fromRenderPrec,
    fromRenderSection,
    fromRenderAndLocated
  )
where

import Data.Loc
import Data.Text (Text)
import Data.Text.Prettyprint.Doc
import qualified Data.Text.Prettyprint.Doc.Render.Text as Text
import Prelude hiding (Ordering (..))
import Render
import qualified Data.Text as Text

docToText :: Doc ann -> Text
docToText = Text.renderStrict . layoutCompact

toText :: Pretty a => a -> Text
toText = docToText . pretty

docToString :: Doc ann -> String
docToString = Text.unpack . docToText

toString :: Pretty a => a -> String
toString = Text.unpack . toText


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
-- toDoc (DocWithLoc d a _) = fillGap (Pos (posFile a) 1 0 0) a <> d
toDoc (DocWithLoc d _ _) = d
toDoc Empty = mempty

-- | If something can be rendered, then make it a Doc
fromRender :: Render a => a -> Doc ann
fromRender x = pretty (render x)

-- | If something can be rendered with precedence, then make it a Doc
fromRenderPrec :: Render a => Int -> a -> Doc ann
fromRenderPrec n x = pretty (renderPrec n x)

-- | If something can be rendered and located, then make it a DocWithLoc
fromRenderAndLocated :: (Located a, Render a) => a -> DocWithLoc ann
fromRenderAndLocated x = case locOf x of
  NoLoc -> mempty
  Loc a b -> DocWithLoc (pretty (render x)) a b

-- | If something can be rendered, then make it a Doc
fromRenderSection :: RenderSection a => a -> Doc ann
fromRenderSection x = pretty (renderSection x)

-- generates newlines and spaces to fill the gap between to Pos
fillGap :: Pos -> Pos -> Doc ann
fillGap this next =
  let lineDiff = posLine next - posLine this
   in if lineDiff == 0
        then -- on the same line, just pad them with spaces

          let colDiff = posCol next - posCol this
           in mconcat (replicate colDiff space)
        else -- on different lines
          mconcat (replicate lineDiff "\n" ++ replicate (posCol next - 1) space)

--------------------------------------------------------------------------------

-- | Pretty print with Precedence
class PrettyPrec a where
  prettyPrec :: Int -> a -> Doc ann

class PrettyWithLoc a where
  prettyWithLoc :: a -> DocWithLoc ann

instance (PrettyWithLoc a, PrettyWithLoc b) => PrettyWithLoc (Either a b) where
  prettyWithLoc (Left x) = prettyWithLoc x
  prettyWithLoc (Right x) = prettyWithLoc x

instance (PrettyPrec a, PrettyPrec b) => PrettyPrec (Either a b) where
  prettyPrec i (Left x) = prettyPrec i x
  prettyPrec i (Right x) = prettyPrec i x

instance (Pretty a, Pretty b) => Pretty (Either a b) where
  pretty (Left x) = pretty x
  pretty (Right x) = pretty x

