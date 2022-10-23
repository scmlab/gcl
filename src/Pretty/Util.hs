{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
module Pretty.Util
  ( docToText
  , toText
  , docToByteString
  , toByteString
  , docToString
  , toString
  , prefixSpaces
  , PrettyPrec(..)
  , PrettyWithLoc(..)
  , DocWithLoc(..)
  , toDoc
  , fromDoc
  , fromRender
  , fromRenderPrec
  , fromRenderSection
  , fromRenderAndLocated
  , VList(..)
  ) where

import           Data.ByteString.Lazy           ( ByteString )
import qualified Data.ByteString.Lazy          as BSL
import           Data.Loc
import           Data.Text                      ( Text )
import qualified Data.Text                     as Text
import qualified Data.Text.Encoding            as Text
import           Prelude                 hiding ( Ordering(..) )
import           Prettyprinter
import qualified Prettyprinter.Render.Text     as Text
import           Render.Class                   ( Render(..)
                                                , RenderSection(renderSection)
                                                , PrecContext(..)
                                                )
-- import           Render

docToText :: Doc ann -> Text
docToText = Text.renderStrict . layoutPretty defaultLayoutOptions

toText :: Pretty a => a -> Text
toText = docToText . pretty

docToByteString :: Doc ann -> ByteString
docToByteString = BSL.fromStrict . Text.encodeUtf8 . docToText

toByteString :: Pretty a => a -> ByteString
toByteString = docToByteString . pretty

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
append Empty              Empty              = Empty
append Empty              (DocWithLoc y c d) = DocWithLoc y c d
append (DocWithLoc x a b) Empty              = DocWithLoc x a b
append (DocWithLoc x a b) (DocWithLoc y c d) = if c >= b
  then DocWithLoc (x <> fillGap b c <> y) a d
  else DocWithLoc (y <> fillGap c b <> x) c b

instance Semigroup (DocWithLoc ann) where
  (<>) = append

instance Monoid (DocWithLoc ann) where
  mappend = (<>)
  mempty  = Empty

fromDoc :: Loc -> Doc ann -> DocWithLoc ann
fromDoc NoLoc     _ = Empty
fromDoc (Loc a b) x = DocWithLoc x a b

-- prefixing spaces are ignored before converting to `Doc` 
toDoc :: DocWithLoc ann -> Doc ann
toDoc (DocWithLoc d _ _) = d
toDoc Empty              = mempty

prefixSpaces :: DocWithLoc ann -> DocWithLoc ann
prefixSpaces (DocWithLoc d x y) =
  let start = Pos (posFile x) 1 1 0
  in  DocWithLoc (fillGap start x <> d) start y
prefixSpaces Empty = mempty

-- | If something can be rendered, then make it a Doc
fromRender :: Render a => a -> Doc ann
fromRender x = pretty (render x)

-- | If something can be rendered with precedence, then make it a Doc
fromRenderPrec :: Render a => PrecContext -> a -> Doc ann
fromRenderPrec n x = pretty (renderPrec n x)

-- | If something can be rendered and located, then make it a DocWithLoc
fromRenderAndLocated :: (Located a, Render a) => a -> DocWithLoc ann
fromRenderAndLocated x = case locOf x of
  NoLoc   -> mempty
  Loc a b -> DocWithLoc (pretty (render x)) a b

-- | If something can be rendered, then make it a Doc
fromRenderSection :: RenderSection a => a -> Doc ann
fromRenderSection x = pretty (renderSection x)

-- generates newlines and spaces to fill the gap between 2 Pos
fillGap :: Pos -> Pos -> Doc ann
fillGap this next =
  let lineDiff = posLine next - posLine this
  in  if lineDiff == 0
        then -- on the same line, just pad them with spaces

          let colDiff = posCol next - posCol this
          in  mconcat (replicate colDiff space)
        else -- on different lines
             mconcat
          (replicate lineDiff "\n" ++ replicate (posCol next - 1) space)

--------------------------------------------------------------------------------

-- | Pretty print with Precedence
class PrettyPrec a where
  prettyPrec :: PrecContext -> a -> Doc ann

class PrettyWithLoc a where
  prettyWithLoc :: a -> DocWithLoc ann

instance (PrettyWithLoc a, PrettyWithLoc b) => PrettyWithLoc (Either a b) where
  prettyWithLoc (Left  x) = prettyWithLoc x
  prettyWithLoc (Right x) = prettyWithLoc x

instance (PrettyPrec a, PrettyPrec b) => PrettyPrec (Either a b) where
  prettyPrec i (Left  x) = prettyPrec i x
  prettyPrec i (Right x) = prettyPrec i x

instance (Pretty a, Pretty b) => Pretty (Either a b) where
  pretty (Left  x) = pretty x
  pretty (Right x) = pretty x

instance PrettyWithLoc a => PrettyWithLoc [a] where
  prettyWithLoc = mconcat . map prettyWithLoc

instance (Pretty a) => PrettyWithLoc (L a) where
  prettyWithLoc (L loc x) = fromDoc loc (pretty x)

--------------------------------------------------------------------------------

-- datatype for printing a list of items vertically without delimiters and enclosings
newtype VList a = VList [a]

instance Pretty a => Pretty (VList a) where
  pretty (VList xs) = vcat (map pretty xs)
