module Server.Util where

import Data.Loc
import Data.Loc.Range
import qualified Data.Text as Text
import qualified Language.LSP.Types as J

toRange :: Range -> J.Range
toRange (Range start end) = J.Range (toPos start) (toPos end)

toPos :: Pos -> J.Position
toPos (Pos _path ln col _offset) = J.Position ((ln - 1) `max` 0) ((col - 1) `max` 0)

toLoc :: Range -> J.Location
toLoc (Range start end) =
  J.Location (J.Uri $ Text.pack $ posFile start) (toRange (Range start end))
