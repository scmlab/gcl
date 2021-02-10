{-# LANGUAGE RecordWildCards #-}
module Syntax.Parser.Util where

import Data.Text.Lazy (Text)
import Text.Megaparsec (setParserState, MonadParsec(getParserState),  unPos, SourcePos(..), errorOffset, Stream(..), ParseError(..), ParseErrorBundle(..), ShowErrorComponent, PosState(..), State (..))
import Text.Megaparsec.Error (parseErrorTextPretty)
import Data.Loc (Loc(..))
import qualified Data.Loc as Loc
-- import Syntax.Concrete

-- (<.*>) :: Applicative f => (f a -> f (b -> c)) -> f b -> (f a -> f c)
-- (<.*>) g fb fa = g fa <*> fb

(<**>) :: Applicative f => (f a -> f (b -> c)) -> (f a -> f b) -> (f a -> f c)
(<**>) ff fg fa = ff fa <*> fg fa

-- (<*.>) :: Applicative f => f (b -> c) -> (f a -> f b) -> (f a -> f c)
-- (<*.>) ff g fa = ff <*> g fa

(<$.>) :: Functor f => (b -> c) -> (f a -> f b) -> (f a -> f c)
(<$.>) g ff = fmap g . ff

type SyntacticError = (Loc, String)

getSourcePos' :: (MonadParsec e s m) => m SourcePos
getSourcePos' = do
  st <- getParserState 
  let pst = reachOffsetNoLine (stateOffset st - 1) (statePosState st)
  setParserState st {statePosState = pst}
  return (pstateSourcePos pst)

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