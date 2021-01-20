{-# LANGUAGE OverloadedStrings #-}

module Pretty.Util where

import           Data.Text.Prettyprint.Doc
import           Prelude                 hiding ( Ordering(..) )
import           Data.Loc
import Data.String (IsString(..))
import Data.Text (pack, Text)
import qualified Data.Text as Text
import qualified Data.Text.Prettyprint.Doc.Render.Text as Text

instance Pretty Loc where
  pretty = pretty . displayLoc


renderStrict :: Doc ann -> Text
renderStrict = Text.renderStrict . layoutPretty defaultLayoutOptions

--------------------------------------------------------------------------------
-- | srcloc related

translate :: Int -> Pos -> Pos
translate n (Pos p l c o) = Pos p l (c + n) (o + n)

translateLoc :: Int -> Int -> Loc -> Loc
translateLoc _ _ NoLoc     = NoLoc
translateLoc m n (Loc x y) = Loc (translate m x) (translate n y)


--------------------------------------------------------------------------------
-- | Prettifier that respects Locs

data DocWithLoc ann 
  -- | A piece of Doc with starting and ending Position
  = DocWithLoc (Doc ann) Pos Pos 
  -- | A piece of Test without any srcloc
  | StringLiteral Text
  deriving Show

-- | Appends two DocWithLoc in a srcloc-respecting way
append :: DocWithLoc ann -> DocWithLoc ann -> DocWithLoc ann
append (StringLiteral s)  (StringLiteral t)  = StringLiteral (s <> t)
append (StringLiteral s)  (DocWithLoc y c d) = DocWithLoc (pretty s <> y) (translate (- (Text.length s)) c) d
append (DocWithLoc x a b) (StringLiteral t)  = DocWithLoc (x <> pretty t) a (translate (Text.length t) b)
append (DocWithLoc x a b) (DocWithLoc y c d) = 
  if c >= b
    then DocWithLoc (x <> fillGap b c <> y) a d
    else DocWithLoc (y <> fillGap c b <> x) c b
  
instance Semigroup (DocWithLoc ann) where   
  (<>) = append

instance Monoid (DocWithLoc ann) where 
  mappend = (<>) 
  mempty = StringLiteral mempty 

-- | With -XOverloadedStrings, we can express `StringLiteral s` with actual string literal
instance IsString (DocWithLoc ann) where 
  fromString s = StringLiteral (pack s)

fromDoc :: Doc ann -> DocWithLoc ann
fromDoc x = StringLiteral (renderStrict x)

fromLocAndDoc :: Loc -> Doc ann -> DocWithLoc ann
fromLocAndDoc NoLoc     x = fromDoc x
fromLocAndDoc (Loc a b) x = DocWithLoc x a b

toDoc :: DocWithLoc ann -> Doc ann 
toDoc (DocWithLoc d a _) = fillGap (Pos (posFile a) 1 1 1) a <> d
toDoc (StringLiteral s) = pretty s

setLoc :: Loc -> DocWithLoc ann -> DocWithLoc ann
setLoc NoLoc     (DocWithLoc x _ _) = StringLiteral (renderStrict x)
setLoc (Loc a b) (DocWithLoc x _ _) = DocWithLoc x a b
setLoc NoLoc     (StringLiteral s)  = StringLiteral s
setLoc (Loc a b) (StringLiteral s)  = DocWithLoc (pretty s) a b

-- generates newlines and spaces to fill the gap between to Pos
fillGap :: Pos -> Pos -> Doc ann 
fillGap this next =
  let lineDiff = posLine next - posLine this
  in if lineDiff == 0 
      -- on the same line, just pad them with spaces
      then let offsetDiff = posCoff next - 1 - posCoff this
          in  mconcat (replicate offsetDiff space) 
      -- on different lines
      else mconcat (replicate lineDiff "\n" ++ replicate (posCol next - 1) space)


--------------------------------------------------------------------------------
-- | Handy combinators

parensIf' :: Int -> Int -> DocWithLoc ann -> DocWithLoc ann 
parensIf' n m doc 
  | n > m     = case doc of 
                  DocWithLoc x a b -> DocWithLoc (parens x) (translate (-1) a) (translate 1 b)
                  StringLiteral  s -> StringLiteral ("(" <> s <> ")")
  | otherwise = doc

sepBy :: DocWithLoc ann -> [DocWithLoc ann] -> DocWithLoc ann 
sepBy _    []       = mempty 
sepBy _    [x]      = x
sepBy deli (x:y:xs) = x <> deli <> sepBy deli (y:xs)

--------------------------------------------------------------------------------
-- | Pretty print with Precedence

class PrettyPrec a where
  prettyPrec :: Int -> a -> Doc ann

class PrettyWithLoc a where
  prettyWithLoc :: a -> DocWithLoc ann

class PrettyPrecWithLoc a where
  prettyPrecWithLoc :: Int -> a -> DocWithLoc ann
