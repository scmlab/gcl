{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}

module Render.Element
  ( Section(..)
  , Block(..)
  , Deco(..)
  , isEmpty
  -- , proofObligationE
  -- , specE
  , Inlines(..)
  , textE
  , codeE
  , linkE
  , substE
  , parensE
  , iconE
  , horzE
  , vertE
  ,
    -- combinators
    (<+>)
  , punctuateAfterE
  , punctuateE
  , sepByCommaE
  ) where

import           Data.Aeson                     ( ToJSON(toJSON) )
import           Data.Foldable                  ( toList )
import           Data.Loc.Range
import           Data.Sequence                  ( Seq(..) )
import qualified Data.Sequence                 as Seq
import           Data.String                    ( IsString(..) )
import           Data.Text                      ( Text )
import qualified Data.Text                     as Text
import           Prettyprinter      ( Pretty(..) )
import qualified Prettyprinter     as Pretty
import           GHC.Generics                   ( Generic )

--------------------------------------------------------------------------------

-- | For decorating a block 
data Deco = Plain | Red | Yellow | Blue | Green
  deriving (Eq, Generic)
instance ToJSON Deco

-- | Basic unit for representing something like a PO or a Spec
--   A Section is comprised of a series of Block elements
data Section = Section Deco [Block]
  deriving (Eq, Generic)
instance ToJSON Section

instance Pretty Section where
  pretty (Section _ blocks) = Pretty.vsep (map pretty blocks)

--------------------------------------------------------------------------------

-- | Block elements
data Block
  -- Plain header with optional location
  = Header Text (Maybe Range)
  -- Special header for Proof Obligations
  | HeaderWithButtons
      Text -- Header text
      (Maybe Range) -- Header location 
      Text -- Anchor hash
      (Maybe Range) -- Anchor location
  | Paragraph Inlines
  | Code Inlines
  deriving (Eq, Generic)

-- Represent Block with String literals
instance IsString Block where
  fromString s = Paragraph (fromString s)

instance Pretty Block where
  pretty (Header header Nothing     ) = pretty header
  pretty (Header header (Just range)) = pretty header <> " at " <> pretty range
  pretty (HeaderWithButtons header Nothing hash Nothing) =
    pretty header <> " #" <> pretty hash
  pretty (HeaderWithButtons header Nothing hash (Just anchor)) =
    pretty header <> " #" <> pretty hash <> " anchored at " <> pretty anchor
  pretty (HeaderWithButtons header (Just range) hash Nothing) =
    pretty header <> " at " <> pretty range <> " #" <> pretty hash
  pretty (HeaderWithButtons header (Just range) hash (Just anchor)) =
    pretty header
      <> " at "
      <> pretty range
      <> " #"
      <> pretty hash
      <> " anchored at "
      <> pretty anchor
  pretty (Paragraph inlines) = pretty inlines
  pretty (Code      inlines) = "`" <> pretty inlines <> "`"

instance ToJSON Block

--------------------------------------------------------------------------------

-- | Datatype for representing a consecutive series of inline elements
newtype Inlines = Inlines {unInlines :: Seq Inline}
  deriving (Eq, Ord)

-- Represent Inlines with String literals
instance IsString Inlines where
  fromString s = Inlines (Seq.singleton (Text (Text.pack s) mempty))

-- | You can join two Inlines using `(<>)`
instance Semigroup Inlines where
  Inlines as <> Inlines bs = Inlines (merge as bs)
   where
    merge :: Seq Inline -> Seq Inline -> Seq Inline
    merge Empty      ys = ys
    merge (xs :|> x) ys = merge xs (cons x ys)

    cons :: Inline -> Seq Inline -> Seq Inline
    cons (Text s c) (Text t d :<| xs) |
      -- merge 2 adjacent Text if they have the same classnames
                                        c == d    = Text (s <> t) c :<| xs
                                      | otherwise = Text s c :<| Text t d :<| xs
    cons (Text s c) (Horz [] :<| xs) = cons (Text s c) xs
    cons (Text s c) (Horz (Inlines t : ts) :<| xs) =
      -- merge Text with Horz when possible
      Horz (Inlines (cons (Text s c) t) : ts) :<| xs
    cons x xs = x :<| xs

instance Monoid Inlines where
  mempty = Inlines mempty

instance ToJSON Inlines where
  toJSON (Inlines xs) = toJSON xs

instance Pretty Inlines where
  pretty = Pretty.hcat . map pretty . insertSpaces . toList . unInlines
   where
      -- insert space before and after inline code snippets 
    insertSpaces :: [Inline] -> [Inline]
    insertSpaces xs = xs >>= \case
      Snpt x -> [Text " " [], Snpt x, Text " " []]
      others -> [others]

instance Show Inlines where
  show = show . pretty

-- | To see if the rendered text is "empty"
isEmpty :: Inlines -> Bool
isEmpty inlines = all elemIsEmpty (Seq.viewl (unInlines inlines))
 where
  elemIsEmpty :: Inline -> Bool
  elemIsEmpty (Icon _  _  ) = False
  elemIsEmpty (Text "" _  ) = True
  elemIsEmpty (Snpt xs    ) = isEmpty xs
  elemIsEmpty (Text _ _   ) = False
  elemIsEmpty (Link _ xs _) = all elemIsEmpty $ unInlines xs
  elemIsEmpty (Sbst _ xs  ) = all elemIsEmpty $ unInlines xs
  elemIsEmpty (Horz xs    ) = all isEmpty xs
  elemIsEmpty (Vert xs    ) = all isEmpty xs
  elemIsEmpty (Parn _     ) = False
  elemIsEmpty (PrHz _     ) = False

infixr 6 <+>

-- | Like `<>` but with a space in between
(<+>) :: Inlines -> Inlines -> Inlines
x <+> y | isEmpty x = y
        | isEmpty y = x
        | otherwise = x <> " " <> y

-- |
textE :: Text -> Inlines
textE s = Inlines $ Seq.singleton $ Text s mempty

-- | Mark a piece of inline elements as inline code
codeE :: Inlines -> Inlines
codeE xs = Inlines $ Seq.singleton $ Snpt xs

-- | Text with source location
linkE :: Range -> Inlines -> Inlines
linkE range xs = Inlines $ Seq.singleton $ Link range xs []

substE :: Int -> Inlines -> Inlines
substE i expr = Inlines $ Seq.singleton $ Sbst i expr

-- | Note: when there's only 1 Horz inside a Parn, convert it to PrHz
parensE :: Inlines -> Inlines
parensE (Inlines (Horz xs :<| Empty)) = Inlines $ Seq.singleton $ PrHz xs
parensE others                        = Inlines $ Seq.singleton $ Parn others

iconE :: String -> Inlines
iconE s = Inlines $ Seq.singleton $ Icon s []

-- | Horizontal listing
horzE :: [Inlines] -> Inlines
horzE = Inlines . pure . Horz

-- | Vertical listing
vertE :: [Inlines] -> Inlines
vertE = Inlines . pure . Vert

punctuateAfterE :: Inlines -> [Inlines] -> [Inlines]
punctuateAfterE _ [] = []
punctuateAfterE delim xs =
  zipWith (<>) xs (replicate (length xs - 1) delim ++ [mempty])

punctuateE :: Inlines -> [Inlines] -> Inlines
punctuateE delim = horzE . punctuateAfterE delim

sepByCommaE :: [Inlines] -> Inlines
sepByCommaE = punctuateE ","

--------------------------------------------------------------------------------

type ClassNames = [String]

-- | Internal type, to be converted to JSON values
data Inline
  = Icon String ClassNames
  | Text Text ClassNames
  -- | "Snippet" for inline code 
  | Snpt Inlines
  | Link Range Inlines ClassNames
  | -- | For Substitution
    Sbst Int Inlines
  | -- | Horizontal grouping, wrap when there's no space
    Horz [Inlines]
  | -- | Vertical grouping, each children would end with a newline
    Vert [Inlines]
  | -- | Parenthese
    Parn Inlines
  | -- | Parenthese around a Horizontal, special case
    PrHz [Inlines]
  deriving (Eq, Ord, Generic)

instance ToJSON Inline

instance Show Inline where
  show = show . pretty

instance Pretty Inline where
  pretty (Icon _ _   ) = mempty
  pretty (Text s _   ) = pretty s
  pretty (Snpt s     ) = pretty s
  pretty (Link _ xs _) = pretty xs
  pretty (Sbst _i xs ) = pretty xs
  pretty (Horz xs    ) = Pretty.hcat (map pretty $ toList xs)
  pretty (Vert xs    ) = Pretty.vcat (map pretty $ toList xs)
  pretty (Parn x     ) = "(" <> pretty x <> ")"
  pretty (PrHz xs    ) = "(" <> Pretty.hcat (map pretty $ toList xs) <> ")"

