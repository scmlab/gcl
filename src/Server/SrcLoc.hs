-- module for LSP <=> GCL srcloc convertion

module Server.SrcLoc
  ( ToOffset(..)
  , makeToOffset
  , toOffset
  , FromOffset(..)
  , makeFromOffset
  , fromOffset
  , fromLSPRange
  , fromLSPPosition
  , toLSPLocation
  , toLSPRange
  , toLSPPosition
  , fromLSPRangeWithoutCharacterOffset
  ) where

import           Data.IntMap                    ( IntMap )
import qualified Data.IntMap                   as IntMap
import           Data.Loc
import           Data.Loc.Range
import           Data.Text                      ( Text )
import qualified Data.Text                     as Text
import qualified Language.LSP.Types            as J

-- Note:  LSP srclocs are 0-base
--        Data.Loc/Data.Range srclocs are 1-base

--------------------------------------------------------------------------------
-- | LSP source locations => Data.Loc/Data.Range source locations

-- | LSP Range -> Data.Range.Range
fromLSPRange :: ToOffset -> FilePath -> J.Range -> Range
fromLSPRange table filepath (J.Range start end) = Range
  (fromLSPPosition table filepath start)
  (fromLSPPosition table filepath end)

-- | LSP Position -> Data.Loc.Pos
fromLSPPosition :: ToOffset -> FilePath -> J.Position -> Pos
fromLSPPosition table filepath (J.Position line col) = Pos
  filepath
  (fromIntegral line + 1) -- starts at 1
  (fromIntegral col + 1) -- starts at 1 
  (fromIntegral (toOffset table (line, col))) -- starts at 0

fromLSPRangeWithoutCharacterOffset :: FilePath -> J.Range -> Range
fromLSPRangeWithoutCharacterOffset filepath (J.Range start end) = Range
  (fromLSPPositionWithoutCharacterOffset filepath start)
  (fromLSPPositionWithoutCharacterOffset filepath end)

-- | LSP Position -> Data.Loc.Pos
fromLSPPositionWithoutCharacterOffset :: FilePath -> J.Position -> Pos
fromLSPPositionWithoutCharacterOffset filepath (J.Position line col) = Pos
  filepath
  (fromIntegral line + 1) -- starts at 1
  (fromIntegral col + 1) -- starts at 1 
  (-1)     -- discard this field

toLSPLocation :: Range -> J.Location
toLSPLocation (Range start end) =
  J.Location (J.Uri $ Text.pack $ posFile start) (toLSPRange (Range start end))

toLSPRange :: Range -> J.Range
toLSPRange (Range start end) = J.Range (toLSPPosition start) (toLSPPosition end)

toLSPPosition :: Pos -> J.Position
toLSPPosition (Pos _path ln col _offset) = J.Position ((ln - 1) `max` 0) ((col - 1) `max` 0)

--------------------------------------------------------------------------------
-- | Positon => Offset convertion

-- Keeps record of offsets of every line break ("\n", "\r" and "\r\n")
--
--  Example text      corresponding entry of IntMap        
--  >abc\n               (1, 4)
--  >def123\r\n          (2, 11)
--  >ghi\r               (3, 15)
--
newtype ToOffset = ToOffset { unToOffset :: IntMap Int }

data Accum = Accum
  { _accumPreviousChar  :: Maybe Char
  , _accumCurrentOffset :: Int
  , _accumCurrentLine   :: Int
  , accumResult        :: IntMap Int
  }

-- | Return a list of offsets of linebreaks ("\n", "\r" or "\r\n")
makeToOffset :: Text -> ToOffset
makeToOffset = ToOffset . accumResult . Text.foldl' go initAccum
 where
  initAccum :: Accum
  initAccum = Accum Nothing 0 0 IntMap.empty

  go :: Accum -> Char -> Accum
  go (Accum (Just '\r') n l table) '\n' =
    Accum (Just '\n') (1 + n) l (IntMap.updateMax (Just . succ) table)
  go (Accum _previous n l table) '\n' =
    Accum (Just '\n') (1 + n) (1 + l) (IntMap.insert (1 + l) (1 + n) table)
  go (Accum _previous n l table) '\r' =
    Accum (Just '\r') (1 + n) (1 + l) (IntMap.insert (1 + l) (1 + n) table)
  go (Accum _previous n l table) char = Accum (Just char) (1 + n) l table

-- | (line, col) => offset (zero-based)
toOffset :: ToOffset -> (Int, Int) -> Int
toOffset (ToOffset table) (line, col) = case IntMap.lookup line table of
  Nothing     -> col
  Just offset -> offset + col

--------------------------------------------------------------------------------
-- | Offset => Position convertion

-- An IntMap for speeding up Offset => Position convertion
-- Keeps record of offsets of every line break ("\n", "\r" and "\r\n")
--
--  Example text      corresponding entry of IntMap        
--  >abc\n               (4, 1)
--  >def123\r\n          (11, 2)
--  >ghi\r               (15, 3)
--
newtype FromOffset = FromOffset { unFromOffset :: IntMap Int }

fromOffset :: FromOffset -> Int -> (Int, Int)
fromOffset (FromOffset table) offset = case IntMap.lookupLE offset table of
  Nothing                          -> (0, offset) -- no previous lines
  Just (offsetOfFirstChar, lineNo) -> (lineNo, offset - offsetOfFirstChar)

makeFromOffset :: Text -> FromOffset
makeFromOffset = FromOffset . accumResult . Text.foldl'
  go
  (Accum Nothing 0 0 IntMap.empty)
 where
  go :: Accum -> Char -> Accum
  -- encountered a "\r\n", update the latest entry 
  go (Accum (Just '\r') n l table) '\n' = case IntMap.deleteFindMax table of
    ((offset, lineNo), table') ->
      Accum (Just '\n') (1 + n) l (IntMap.insert (1 + offset) lineNo table')
  -- encountered a line break, add a new entry 
  go (Accum _previous n l table) '\n' =
    Accum (Just '\n') (1 + n) (1 + l) (IntMap.insert (1 + n) (1 + l) table)
  go (Accum _previous n l table) '\r' =
    Accum (Just '\r') (1 + n) (1 + l) (IntMap.insert (1 + n) (1 + l) table)
  go (Accum _previous n l table) char = Accum (Just char) (1 + n) l table
