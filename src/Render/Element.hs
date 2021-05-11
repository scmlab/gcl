{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}

module Render.Element
  ( Block (..),
    Inlines (..),
    space,
    text,
    text',
    parens,
    -- link,
    icon,
    -- combinators
    (<+>),
    punctuate,
  )
where

import Data.Aeson (ToJSON (toJSON))
import Data.Foldable (toList)
import Data.Loc.Range
import Data.Sequence (Seq (..))
import qualified Data.Sequence as Seq
import Data.String (IsString (..))
import GHC.Generics (Generic)

--------------------------------------------------------------------------------

data Block
  = Unlabeled Inlines (Maybe String) (Maybe Range)
  | -- headers
    Header String
  deriving (Eq, Generic)

instance ToJSON Block 

--------------------------------------------------------------------------------

newtype Inlines = Inlines {unInlines :: Seq Inline}
  deriving (Eq)

-- Represent Inlines with String literals
instance IsString Inlines where
  fromString s = Inlines (Seq.singleton (Text s mempty))

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
    -- elemIsEmpty (Link _ xs _) = all elemIsEmpty $ unInlines xs
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

-- | Whitespace
space :: Inlines
space = " "

text :: String -> Inlines
text s = Inlines $ Seq.singleton $ Text s mempty

-- | `text` with `ClassNames`
text' :: ClassNames -> String -> Inlines
text' cs s = Inlines $ Seq.singleton $ Text s cs

-- When there's only 1 Horz inside a Parn, convert it to PrHz
parens :: Inlines -> Inlines
parens (Inlines (Horz xs :<| Empty)) = Inlines $ Seq.singleton $ PrHz xs
parens others = Inlines $ Seq.singleton $ Parn others

icon :: String -> Inlines
icon s = Inlines $ Seq.singleton $ Icon s []

-- linkHole :: Int -> Inlines
-- linkHole i = Inlines $ Seq.singleton $ Hole i

--------------------------------------------------------------------------------

type ClassNames = [String]

--------------------------------------------------------------------------------

-- | Internal type, to be converted to JSON values
data Inline
  = Icon String ClassNames
  | Text String ClassNames
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
  show (Text s _) = s
  -- show (Link _ xs _) = mconcat (map show $ toList $ unInlines xs)
  show (Horz xs) = unwords (map show $ toList xs)
  show (Vert xs) = unlines (map show $ toList xs)
  show (Parn x) = "(" <> show x <> ")"
  show (PrHz xs) = "(" <> unwords (map show $ toList xs) <> ")"

--------------------------------------------------------------------------------

-- | Utilities / Combinators
punctuate :: Inlines -> [Inlines] -> [Inlines]
punctuate _ [] = []
punctuate delim xs = zipWith (<>) xs (replicate (length xs - 1) delim ++ [mempty])

--------------------------------------------------------------------------------
