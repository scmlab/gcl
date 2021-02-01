{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Syntax.Parser.Util2 where

import Data.Text.Lazy (Text)
import Text.Megaparsec (errorOffset, Stream(..), ParseError(..), ParseErrorBundle(..), ShowErrorComponent, (<?>), sepBy1, unPos, SourcePos(..), PosState(..), State(..), MonadParsec(..))
import Text.Megaparsec.Error (parseErrorTextPretty, attachSourcePos, ErrorItem(..))
import Data.Loc ((<-->), Loc(..))
import qualified Data.Loc as Loc
import Syntax.Concrete
import Syntax.Parser.Lexer2
import Syntax.Parser (SyntacticError)

-- parse one or more elements into a list
pToList :: Lexer a -> Lexer [a]
pToList meth = sepBy1 meth tokComma <?> "a list of elements seperated by commas"

withLoc :: Lexer (Loc -> a) -> Lexer a
withLoc m = do
  State {statePosState = PosState {pstateSourcePos = start}} <- getParserState
  f <- m
  State {statePosState = PosState {pstateSourcePos = end}} <- getParserState
  return . f $ sourceToLoc start <--> sourceToLoc end

getLoc :: Lexer a -> Lexer (a, Loc)
getLoc m = do
  State {statePosState = PosState {pstateSourcePos = start}} <- getParserState
  x <- m
  State {statePosState = PosState {pstateSourcePos = end}} <- getParserState
  return (x, sourceToLoc start <--> sourceToLoc end)


textToName :: Lexer Text -> Lexer Name
textToName m = withLoc (Name <$> m)

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