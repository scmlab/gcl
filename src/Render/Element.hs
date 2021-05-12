{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}

module Render.Element
  ( Block,
    blockE,
    proofObligationE,
    headerE,
    Inlines (..),
    textE,
    textE',
    parensE,
    linkE,
    iconE,
    horzE, 
    vertE,
    -- combinators
    (<+>),
    punctuateE,
  )
where

import Data.Aeson (ToJSON (toJSON))
import Data.Foldable (toList)
import Data.Loc.Range
import Data.Sequence (Seq (..))
import qualified Data.Sequence as Seq
import Data.String (IsString (..))
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Text.Prettyprint.Doc (Pretty (..))
import GHC.Generics (Generic)

--------------------------------------------------------------------------------

data Block
  = -- header + body
    Block (Maybe String) (Maybe Range) Inlines
  | -- precondition + post-condition
    PO (Maybe String) Inlines Inlines
  | -- "naked" header
    Header String
  deriving (Eq, Generic)

-- Represent Block with String literals
instance IsString Block where
  fromString s = Block Nothing Nothing (fromString s)

instance Show Block where
  show (Block Nothing range body) = show body <> "\nat " <> show range
  show (Block (Just header) range body) = "## " <> header <> "\n\n" <> show body <> "\nat " <> show range
  show (PO Nothing pre post) = show pre <> "\n=>\n" <> show post
  show (PO (Just header) pre post) = "# " <> header <> "\n\n" <> show pre <> "\n=>\n" <> show post
  show (Header header) = "# " <> header

instance Pretty Block where
  pretty = pretty . show

instance ToJSON Block

blockE :: Maybe String -> Maybe Range -> Inlines -> Block
blockE = Block

proofObligationE :: Maybe String -> Inlines -> Inlines -> Block
proofObligationE = PO

headerE :: String -> Block
headerE = Header

--------------------------------------------------------------------------------

newtype Inlines = Inlines {unInlines :: Seq Inline}
  deriving (Eq)

-- Represent Inlines with String literals
instance IsString Inlines where
  fromString s = Inlines (Seq.singleton (Text (Text.pack s) mempty))

instance Semigroup Inlines where
  Inlines as <> Inlines bs = Inlines (merge as bs)
    where
      merge :: Seq Inline -> Seq Inline -> Seq Inline
      merge Empty ys = ys
      merge (xs :|> x) ys = merge xs (cons x ys)

      cons :: Inline -> Seq Inline -> Seq Inline
      cons (Text s c) (Text t d :<| xs)
        -- merge 2 adjacent Text if they have the same classnames
        | c == d = Text (s <> t) c :<| xs
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

-- | see if the rendered text is "empty"
isEmpty :: Inlines -> Bool
isEmpty inlines = all elemIsEmpty (Seq.viewl (unInlines inlines))
  where
    elemIsEmpty :: Inline -> Bool
    elemIsEmpty (Icon _ _) = False
    elemIsEmpty (Text "" _) = True
    elemIsEmpty (Text _ _) = False
    elemIsEmpty (Link _ xs _) = all elemIsEmpty $ unInlines xs
    elemIsEmpty (Horz xs) = all isEmpty xs
    elemIsEmpty (Vert xs) = all isEmpty xs
    elemIsEmpty (Parn _) = False
    elemIsEmpty (PrHz _) = False

infixr 6 <+>

(<+>) :: Inlines -> Inlines -> Inlines
x <+> y
  | isEmpty x = y
  | isEmpty y = x
  | otherwise = x <> " " <> y

textE :: Text -> Inlines
textE s = Inlines $ Seq.singleton $ Text s mempty

-- | `text` with `ClassNames`
textE' :: ClassNames -> Text -> Inlines
textE' cs s = Inlines $ Seq.singleton $ Text s cs

-- When there's only 1 Horz inside a Parn, convert it to PrHz
parensE :: Inlines -> Inlines
parensE (Inlines (Horz xs :<| Empty)) = Inlines $ Seq.singleton $ PrHz xs
parensE others = Inlines $ Seq.singleton $ Parn others

iconE :: String -> Inlines
iconE s = Inlines $ Seq.singleton $ Icon s []

linkE :: Range -> Inlines -> Inlines
linkE range xs = Inlines $ Seq.singleton $ Link range xs []

-- | Horizontal listing
horzE :: [Inlines] -> Inlines
horzE = Inlines . pure . Horz

-- | Vertical listing
vertE :: [Inlines] -> Inlines
vertE = Inlines . pure . Vert


-- linkHole :: Int -> Inlines
-- linkHole i = Inlines $ Seq.singleton $ Hole i

--------------------------------------------------------------------------------

type ClassNames = [String]

-- | Internal type, to be converted to JSON values
data Inline
  = Icon String ClassNames
  | Text Text ClassNames
  | Link Range Inlines ClassNames
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
  show (Icon s _) = s
  show (Text s _) = Text.unpack s
  show (Link _ xs _) = mconcat (map show $ toList $ unInlines xs)
  show (Horz xs) = unwords (map show $ toList xs)
  show (Vert xs) = unlines (map show $ toList xs)
  show (Parn x) = "(" <> show x <> ")"
  show (PrHz xs) = "(" <> unwords (map show $ toList xs) <> ")"

--------------------------------------------------------------------------------

-- | Utilities / Combinators
punctuateE :: Inlines -> [Inlines] -> [Inlines]
punctuateE _ [] = []
punctuateE delim xs = zipWith (<>) xs (replicate (length xs - 1) delim ++ [mempty])

--------------------------------------------------------------------------------
