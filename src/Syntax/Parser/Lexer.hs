{-# LANGUAGE OverloadedStrings #-}

module Syntax.Parser.Lexer (scan, Tok(..), TokStream) where

import Syntax.Parser.TokenStream (PrettyToken(..))
import Error (Error)
import qualified Error as Error

import Data.List.NonEmpty (NonEmpty(..))
import qualified Data.List.NonEmpty as NE
import Data.Maybe (fromMaybe)

import Language.Lexer.Applicative
import Text.Regex.Applicative
import Data.Char
-- import Data.Text hiding (Space)
import Data.Text.Lazy (Text)
import qualified Data.Text.Lazy as Text
import Data.Loc

--------------------------------------------------------------------------------
-- | Tok & TokStream

data Tok
    = TokNewline
    | TokWhitespace
    | TokEOF
    | TokComment Text

    -- keywords
    | TokSkip
    | TokAbort
    | TokDo
    | TokOd
    | TokIf
    | TokFi
    | TokBnd

    | TokQM

    | TokVar
    | TokCon

    | TokGuardBar
    | TokGuardArr

    -- delimiters
    | TokComma
    | TokSemi
    | TokAssign
    | TokSpecStart
    | TokSpecEnd
    | TokParenStart
    | TokParenEnd
    | TokBraceStart
    | TokBraceEnd

    -- literals
    | TokEQ
    | TokGT
    | TokGTE
    | TokLT
    | TokLTE

    | TokImpl
    | TokConj
    | TokDisj
    | TokNeg


    | TokUpperName Text
    | TokLowerName Text
    | TokInt       Int
    | TokTrue
    | TokFalse

    deriving (Eq, Ord)

instance Show Tok where
  show tok = case tok of
    TokNewline -> "\n"
    TokWhitespace -> " "
    TokEOF -> ""
    TokComment s -> "-- " ++ Text.unpack s
    TokSkip -> "skip"
    TokAbort -> "abort"
    TokDo -> "do"
    TokOd -> "od"
    TokIf -> "if"
    TokFi -> "fi"
    TokBnd -> "bnd"
    TokQM -> "?"
    TokVar -> "var"
    TokCon -> "con"
    TokGuardBar -> "|"
    TokGuardArr -> "->"
    TokComma -> ","
    TokSemi -> ":"
    TokAssign -> ":="
    TokSpecStart -> "{!"
    TokSpecEnd -> "!}"
    TokParenStart -> "("
    TokParenEnd -> ")"
    TokBraceStart -> "{"
    TokBraceEnd -> "}"
    TokEQ -> "="
    TokGT -> ">"
    TokGTE -> ">="
    TokLT -> "<"
    TokLTE -> "<="
    TokImpl -> "=>"
    TokConj -> "&&"
    TokDisj -> "||"
    TokNeg -> "^"
    TokUpperName s -> Text.unpack s
    TokLowerName s -> Text.unpack s
    TokInt i -> show i
    TokTrue -> "True"
    TokFalse -> "False"

type TokStream = TokenStream (L Tok)

--------------------------------------------------------------------------------
-- | Regular expressions & the lexer

text :: Text -> RE Text Text
text raw = Text.foldr f (pure "") raw
  where
    f :: Char -> RE Text Text -> RE Text Text
    f c p = Text.cons <$ sym (Text.singleton c) <*> pure c <*> p



tokRE :: RE Text Tok
tokRE
   =
      TokNewline    <$ text "\n"

  <|> TokSkip       <$ text "skip"
  <|> TokAbort      <$ text "abort"
  <|> TokDo         <$ text "do"
  <|> TokOd         <$ text "od"
  <|> TokIf         <$ text "if"
  <|> TokFi         <$ text "fi"
  <|> TokBnd        <$ text "bnd"

  <|> TokQM         <$ text "?"

  <|> TokCon        <$ text "con"
  <|> TokVar        <$ text "var"

  <|> TokGuardBar   <$ text "|"
  <|> TokGuardArr   <$ text "->"

  -- delimiters
  <|> TokComma        <$ text ","
  <|> TokSemi         <$ text ":"
  <|> TokAssign       <$ text ":="
  <|> TokSpecStart    <$ text "{!"
  <|> TokSpecEnd      <$ text "!}"
  <|> TokParenStart   <$ text "("
  <|> TokParenEnd     <$ text ")"
  <|> TokBraceStart   <$ text "{"
  <|> TokBraceEnd     <$ text "}"

  -- literals
  <|> TokEQ           <$ text "="
  <|> TokGT           <$ text ">"
  <|> TokGTE          <$ text ">="
  <|> TokLT           <$ text "<"
  <|> TokLTE          <$ text "<="
  <|> TokImpl         <$ text "=>"
  <|> TokConj         <$ text "&&"
  <|> TokDisj         <$ text "||"
  <|> TokNeg          <$ text "^"

  <|> TokTrue         <$ text "True"
  <|> TokFalse        <$ text "False"

  <|> TokUpperName    <$> upperNameRE
  <|> TokLowerName    <$> lowerNameRE
  <|> TokInt          <$> intRE

adapt :: (Char -> Bool) -> Text -> Bool
adapt f xs
  | Text.null xs = False
  | otherwise    = f (Text.head xs)

-- starts with lowercase alphabets
lowerNameRE :: RE Text Text
lowerNameRE = Text.append
  <$> psym (adapt isLower)
  <*> (Text.concat <$> many (psym (adapt (\c -> isAlphaNum c || c == '_' || c == '\''))))

-- starts with uppercase alphabets
upperNameRE :: RE Text Text
upperNameRE = Text.append
  <$> psym (adapt isUpper)
  <*> (Text.concat <$> many (psym $ adapt (\c -> isAlphaNum c || c == '_' || c == '\'')))

intRE :: RE Text Int
intRE = read <$> (Text.unpack . Text.concat <$> some (psym (adapt isDigit)))

whitespaceButNewlineRE :: RE Text Tok
-- whitespaceRE = matchWhen isSpace TokWhitespace
whitespaceButNewlineRE = matchWhen (adapt (\c -> isSpace c && c /= '\n' && c /= '\r')) TokWhitespace
  where
    matchWhen :: (Text -> Bool) -> Tok -> RE Text Tok
    matchWhen p symbol = msym (\t -> if p t then Just symbol else Nothing)


commentStartRE :: RE Text Text
commentStartRE = text "--"

commentEndRE :: Text -> RE Text Tok
commentEndRE prefix = TokComment <$> (pure prefix +++ (Text.concat <$> many anySym) +++ text "\n")
  where
    (+++) = liftA2 (<>)

contra :: RE Text a -> RE Char a
contra = comap Text.singleton

lexer :: Lexer Tok
lexer = mconcat
  [ token       (longest $ contra tokRE)
  , whitespace  (longest $ contra whitespaceButNewlineRE)
  , whitespace  (longestShortest (contra commentStartRE) (contra . commentEndRE))
  ]

--------------------------------------------------------------------------------
-- | scan

scan :: FilePath -> Text -> Either [Error] TokStream
scan filepath = filterError . runLexer lexer filepath . Text.unpack
  where
    filterError :: TokStream -> Either [Error] TokStream
    filterError TsEof = Right TsEof
    filterError (TsError (LexicalError pos)) = Left [Error.LexicalError pos]
    filterError (TsToken l xs) = TsToken l <$> filterError xs


--------------------------------------------------------------------------------
-- | Instances of PrettyToken

instance PrettyToken Tok where
  prettyTokens (x:|[])  = fromMaybe ("'" <> show (unLoc x) <> "'") (prettyToken' (unLoc x))
  prettyTokens xs       = "\"" <> concatMap (f . unLoc) (NE.toList xs) <> "\""
    where
      f tok =
        case prettyToken' tok of
          Nothing     -> show tok
          Just pretty -> "<" <> pretty <> ">"

-- | If the given character has a pretty representation, return that,
-- otherwise 'Nothing'. This is an internal helper.

prettyToken' :: Tok -> Maybe String
prettyToken' tok = case tok of
  TokNewline -> Just "newline"
  TokWhitespace -> Just "space"
  TokEOF -> Just "end of file"
  _      -> Nothing
