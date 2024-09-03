{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}

module Data.Loc.Range where

import           Data.Aeson                     ( FromJSON(..)
                                                , ToJSON(..), object, (.=), withObject, (.:)
                                                )
import qualified Data.List                     as List
import           Data.List.NonEmpty             ( NonEmpty )
import qualified Data.List.NonEmpty            as NE
import           Data.Loc                hiding ( fromLoc )
import           Data.Maybe                     ( mapMaybe )
import           GHC.Generics                   ( Generic )
import           Prettyprinter                  ( Pretty(pretty) )

-- | Represents an interval of two source locations
--
--  Very much like `Loc`, except that:
--    1. There's no `NoLoc`
--    2. Cursors is placed IN-BETWEEN two characters rather than ON a character
--
--  For example: to represent the selection of "ABC" in "ABCD" 
--    (here we use the tip of ">" and "<" to represent a cursor between two characters)
--
--    charactor offset    :   0123
--    charactors          :   ABCD   
-------------------------------------------------------
--    Loc       of "ABC"  :   ^^^     Loc   (Pos ... 0) (Pos ... 2)
--    Range     of "ABC"  :  >   <    Range (Pos ... 0) (Pos ... 3)
--
--    Loc       of "AB"   :   ^^      Loc   (Pos ... 0) (Pos ... 1)
--    Range     of "AB"   :  >  <     Range (Pos ... 0) (Pos ... 2)
--
--    Loc       of ""     :  ####### UNREPRESENTABLE ###################
--    Range     of ""     :  ><       Range (Pos ... 0) (Pos ... 0)
--
--  We abuse `Pos` to represent what is actually the left endpoint of that `Pos`

data Range = Range Pos Pos
  deriving (Eq, Generic)

-- First by comparing their starting positions and then their ending positions
instance Ord Range where
  compare (Range a b) (Range c d) = case compare a c of
    EQ     -> compare b d
    others -> others

instance Show Range where
  show (Range start end) = if posLine start == posLine end
    then
      posFile start
      <> " ["
      <> show (posCoff start)
      <> "-"
      <> show (posCoff end)
      <> "] "
      <> show (posLine start)
      <> ":"
      <> show (posCol start)
      <> "-"
      <> show (posCol end)
    else
      posFile start
      <> " ["
      <> show (posCoff start)
      <> "-"
      <> show (posCoff end)
      <> "] "
      <> show (posLine start)
      <> ":"
      <> show (posCol start)
      <> "-"
      <> show (posLine end)
      <> ":"
      <> show (posCol end)

-- | Starting position of the range
rangeStart :: Range -> Pos
rangeStart (Range a _) = a

-- | Ending position of the range
rangeEnd :: Range -> Pos
rangeEnd (Range _ b) = b

-- | Filepath of the range
rangeFile :: Range -> FilePath
rangeFile (Range a _) = posFile a

-- | Loc -> Maybe Range
fromLoc :: Loc -> Maybe Range
fromLoc NoLoc     = Nothing
fromLoc (Loc x y) = Just (Range x y)

-- | Range -> Loc
toLoc :: Range -> Loc
toLoc (Range x y) = Loc x y

-- | [Loc] -> [Range]
fromLocs :: [Loc] -> [Range]
fromLocs = mapMaybe fromLoc

mergeRangesUnsafe :: [Range] -> Range
mergeRangesUnsafe xs = foldl (<>) (head xs) xs

mergeRanges :: NonEmpty Range -> Range
mergeRanges xs = foldl (<>) (NE.head xs) xs

-- | Calculates the length covered by a range
rangeSpan :: Range -> Int
rangeSpan (Range a b) = posCol b - posCol a

-- | See if a Range is within another Range 
within :: Range -> Range -> Bool
within (Range a b) (Range c d) = posCol c <= posCol a && posCol b <= posCol d

instance Located Range where
  locOf (Range x y) = Loc x y

-- | Merge two ranges by filling their gap
instance Semigroup Range where
  Range a b <> Range c d = case (a `compare` c, b `compare` d) of
    (LT, LT) -> Range a d
    (LT, EQ) -> Range a d
    (LT, GT) -> Range a b
    (EQ, LT) -> Range a d
    (EQ, EQ) -> Range a b
    (EQ, GT) -> Range a b
    (GT, LT) -> Range c d
    (GT, EQ) -> Range c d
    (GT, GT) -> Range c b

--------------------------------------------------------------------------------

-- | Like "Located"
class Ranged a where
  rangeOf :: a -> Range

instance Ranged Range where
  rangeOf x = x

instance Ranged a => Ranged (NonEmpty a) where
  rangeOf xs = rangeOf (NE.head xs) <> rangeOf (NE.last xs)

--------------------------------------------------------------------------------

-- | A value of type @R a@ is a value of type @a@ with an associated 'Range', but
-- this location is ignored when performing comparisons.
data R a = R Range a
  deriving Functor

unRange :: R a -> a
unRange (R _ a) = a

instance Eq x => Eq (R x) where
  (R _ x) == (R _ y) = x == y

instance Ord x => Ord (R x) where
  compare (R _ x) (R _ y) = compare x y

instance Show x => Show (R x) where
  show (R _ x) = show x

instance Ranged (R a) where
  rangeOf (R range _) = range

--------------------------------------------------------------------------------

-- | Make Pos instances of FromJSON and ToJSON
instance ToJSON Pos where
  toJSON (Pos file line col byte) =
    object [ "file" .= file
            , "line" .= line
            , "column" .= col
            , "byte" .= byte
            ]

instance FromJSON Pos where
  parseJSON = withObject "Pos" $ \v ->
    Pos <$> v .: "file"
        <*> v .: "line"
        <*> v .: "column"
        <*> v .: "byte"

-- | Make Range instances  of FromJSON and ToJSON
instance FromJSON Range where
  parseJSON = withObject "Range" $ \v ->
    Range <$> v .: "start"
          <*> v .: "end"
instance ToJSON Range where
  toJSON (Range start end) =
    object [ "start" .= start
           , "end" .= end
           ]

--------------------------------------------------------------------------------

-- | Compare the cursor position with something
--  EQ: the cursor is placed within that thing
--  LT: the cursor is placed BEFORE (but not touching) that thing
--  GT: the cursor is placed AFTER (but not touching) that thing
compareWithPosition :: Located a => Pos -> a -> Ordering
compareWithPosition pos x = case locOf x of
  NoLoc         -> EQ
  Loc start end -> if posCoff pos < posCoff start
    then LT
    else if posCoff pos > posCoff end then GT else EQ

-- | See if something is within the selection
withinRange :: Located a => Range -> a -> Bool
withinRange (Range left right) x =
  compareWithPosition left x
    == EQ
    || compareWithPosition right x
    == EQ
    || (compareWithPosition left x == LT && compareWithPosition right x == GT)

--------------------------------------------------------------------------------

instance Pretty Range where
  pretty (Range start end) = if posLine start == posLine end
    then
      pretty (posFile start)
      <> " ["
      <> pretty (posCoff start)
      <> "-"
      <> pretty (posCoff end)
      <> "] "
      <> pretty (posLine start)
      <> ":"
      <> pretty (posCol start)
      <> "-"
      <> pretty (posCol end)
    else
      pretty (posFile start)
      <> " ["
      <> pretty (posCoff start)
      <> "-"
      <> pretty (posCoff end)
      <> "] "
      <> pretty (posLine start)
      <> ":"
      <> pretty (posCol start)
      <> "-"
      <> pretty (posLine end)
      <> ":"
      <> pretty (posCol end)

instance Pretty Loc where
  pretty = pretty . displayLoc

instance Pretty Pos where
  pretty = pretty . displayPos

--------------------------------------------------------------------------------

-- | Like Range but a special  Show &Pretty instance, won't display the full path
newtype ShortRange = ShortRange { unShortRange :: Range }

instance Show ShortRange where
  show (ShortRange (Range start end)) =
    let path = case split '/' (posFile start) of
          [] -> []
          xs -> last xs
    in  if posLine start == posLine end
          then
            path
            <> " ["
            <> show (posCoff start)
            <> "-"
            <> show (posCoff end)
            <> "] "
            <> show (posLine start)
            <> ":"
            <> show (posCol start)
            <> "-"
            <> show (posCol end)
          else
            path
            <> " ["
            <> show (posCoff start)
            <> "-"
            <> show (posCoff end)
            <> "] "
            <> show (posLine start)
            <> ":"
            <> show (posCol start)
            <> "-"
            <> show (posLine end)
            <> ":"
            <> show (posCol end)
   where
    split :: Char -> String -> [String]
    split c = filter (/= [c]) . List.groupBy (\x y -> x /= c && y /= c)

instance Pretty ShortRange where
  pretty = pretty . show
