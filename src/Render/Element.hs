{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}

module Render.Element
  ( Section(..)
  , Block(..)
  , Deco(..)
  -- , proofObligationE
  -- , specE
  , Inlines(..)
  , textE
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
  ) where

import           Data.Aeson                     ( ToJSON(toJSON) )
import           Data.Foldable                  ( toList )
import           Data.Loc.Range
import           Data.Sequence                  ( Seq(..) )
import qualified Data.Sequence                 as Seq
import           Data.String                    ( IsString(..) )
import           Data.Text                      ( Text )
import qualified Data.Text                     as Text
import           Data.Text.Prettyprint.Doc      ( Pretty(..) )
import qualified Data.Text.Prettyprint.Doc     as Pretty
import           GHC.Generics                   ( Generic )

--------------------------------------------------------------------------------

-- | For decorating a block 
data Deco = Plain | Red | Yellow | Blue | Green
  deriving (Eq, Generic)
instance ToJSON Deco

-- | Section
data Section = Section
  { sectionDeco   :: Deco
  , sectionBlocks :: [Block]
  }
  deriving (Eq, Generic)
instance ToJSON Section

instance Pretty Section where
  pretty (Section _ blocks) = Pretty.vsep (map pretty blocks)

--------------------------------------------------------------------------------

-- | Block elements
data Block
  = Header Text (Maybe Range)
  | Paragraph Inlines
  | Code Inlines
  deriving (Eq, Generic)

-- Represent Block with String literals
instance IsString Block where
  fromString s = Paragraph (fromString s)

instance Pretty Block where
  pretty (Header header Nothing     ) = pretty header
  pretty (Header header (Just range)) = pretty header <> " at " <> pretty range
  pretty (Paragraph inlines         ) = pretty inlines
  pretty (Code      inlines         ) = "`" <> pretty inlines <> "`"

instance ToJSON Block

-- -- | Constructor for `PO`
-- proofObligationE :: Maybe String -> Maybe Range -> Inlines -> Block
-- proofObligationE = PO

-- -- | Constructor for `Spec`
-- specE :: Range -> Inlines -> Inlines -> Block
-- specE = Spec

--------------------------------------------------------------------------------

-- | Datatype for representing a consecutive series of inline elements
newtype Inlines = Inlines {unInlines :: Seq Inline}
  deriving (Eq)

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

instance Show Inlines where
  show (Inlines xs) = unwords $ map show $ toList xs

instance Pretty Inlines where
  pretty = pretty . show

-- | To see if the rendered text is "empty"
isEmpty :: Inlines -> Bool
isEmpty inlines = all elemIsEmpty (Seq.viewl (unInlines inlines))
 where
  elemIsEmpty :: Inline -> Bool
  elemIsEmpty (Icon _  _  ) = False
  elemIsEmpty (Text "" _  ) = True
  elemIsEmpty (Text _  _  ) = False
  elemIsEmpty (Link _ xs _) = all elemIsEmpty $ unInlines xs
  elemIsEmpty (Sbst xs env ys _) =
    all elemIsEmpty $ unInlines xs <> unInlines env <> unInlines ys
  elemIsEmpty (Horz xs) = all isEmpty xs
  elemIsEmpty (Vert xs) = all isEmpty xs
  elemIsEmpty (Parn _ ) = False
  elemIsEmpty (PrHz _ ) = False

infixr 6 <+>

-- | Like `<>` but with a space in between
(<+>) :: Inlines -> Inlines -> Inlines
x <+> y | isEmpty x = y
        | isEmpty y = x
        | otherwise = x <> " " <> y

-- |
textE :: Text -> Inlines
textE s = Inlines $ Seq.singleton $ Text s mempty

-- | Text with source location
linkE :: Range -> Inlines -> Inlines
linkE range xs = Inlines $ Seq.singleton $ Link range xs []

-- | Text that changes after clicked
substE :: Inlines -> Inlines -> Inlines -> Inlines
substE before env after = Inlines $ Seq.singleton $ Sbst before env after []

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

--------------------------------------------------------------------------------

type ClassNames = [String]

-- | Internal type, to be converted to JSON values
data Inline
  = Icon String ClassNames
  | Text Text ClassNames
  | Link Range Inlines ClassNames
  | -- | For Subst
    Sbst Inlines Inlines Inlines ClassNames
  | -- | Horizontal grouping, wrap when there's no space
    Horz [Inlines]
  | -- | Vertical grouping, each children would end with a newline
    Vert [Inlines]
  | -- | Parenthese
    Parn Inlines
  | -- | Parenthese around a Horizontal, special case
    PrHz [Inlines]
  deriving (Eq, Generic)

instance ToJSON Inline

instance Show Inline where
  show (Icon s _       ) = s
  show (Text s _       ) = Text.unpack s
  show (Link _ xs _    ) = show xs
  show (Sbst xs env _ _) = show xs <> show env
  show (Horz xs        ) = unwords (map show $ toList xs)
  show (Vert xs        ) = unlines (map show $ toList xs)
  show (Parn x         ) = "(" <> show x <> ")"
  show (PrHz xs        ) = "(" <> unwords (map show $ toList xs) <> ")"

instance Pretty Inline where
  pretty = pretty . show
