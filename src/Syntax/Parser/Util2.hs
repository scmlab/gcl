{-# LANGUAGE RecordWildCards #-}
module Syntax.Parser.Util2 where

import Data.Text.Lazy (Text)
import Text.Megaparsec (unPos, SourcePos(..), errorOffset, Stream(..), ParseError(..), ParseErrorBundle(..), ShowErrorComponent, PosState(..))
import Text.Megaparsec.Error (parseErrorTextPretty)
import Data.Loc (Loc(..))
import qualified Data.Loc as Loc
-- import Syntax.Concrete


type SyntacticError = (Loc, String)

sourceToLoc :: SourcePos -> Loc
sourceToLoc SourcePos{..} = Loc.locOf $ Loc.Pos sourceName (unPos sourceLine) (unPos sourceColumn) 0

posStateToLoc :: Stream s => PosState s -> Loc
posStateToLoc = Loc.locOf . sourceToLoc . pstateSourcePos

fromParseErrorBundle :: ShowErrorComponent e => ParseErrorBundle Text e -> [SyntacticError]
fromParseErrorBundle (ParseErrorBundle errs posState)= 
  snd $ foldr f (posState, []) errs
  where
    f :: ShowErrorComponent e => 
      ParseError Text e 
      -> (PosState Text, [SyntacticError]) 
      -> (PosState Text, [SyntacticError])
    f err (i, acc) = 
      let n = reachOffsetNoLine (errorOffset err) i in                    
        (n, (posStateToLoc n, parseErrorTextPretty err) : acc)