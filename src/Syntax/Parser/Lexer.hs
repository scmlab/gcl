{-# LANGUAGE OverloadedStrings #-}

module Syntax.Parser.Lexer (scan, LexicalError, Tok(..), TokStream) where

import Syntax.Parser.TokenStream (PrettyToken(..))

import Data.Char
import Data.List.NonEmpty (NonEmpty(..))
import qualified Data.List.NonEmpty as NE
import Data.Loc
import Data.Maybe (fromMaybe)
import Data.Text.Lazy (Text)
import qualified Data.Text.Lazy as Text
import Language.Lexer.Applicative hiding (LexicalError)
import qualified Language.Lexer.Applicative as Lex
import Text.Regex.Applicative

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
    | TokArray
    | TokOf
    | TokRange

    | TokGuardBar
    | TokArrow

    -- delimiters
    | TokComma
    | TokSemi
    | TokAssign
    | TokSpecStart
    | TokSpecEnd
    | TokParenStart
    | TokParenEnd
    | TokBracketStart
    | TokBracketEnd
    | TokBraceStart
    | TokBraceEnd

    -- expression

    -- operators
    | TokEQ
    | TokNEQ
    | TokGT
    | TokGTE
    | TokLT
    | TokLTE

    | TokImpl
    | TokConj
    | TokDisj
    | TokNeg

    | TokAdd
    | TokSub
    | TokMul
    | TokDiv
    | TokMod

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
    TokArray -> "array"
    TokOf -> "of"
    TokRange -> ".."
    TokGuardBar -> "|"
    TokArrow -> "->"
    TokComma -> ","
    TokSemi -> ":"
    TokAssign -> ":="
    TokSpecStart -> "{!"
    TokSpecEnd -> "!}"
    TokParenStart -> "("
    TokParenEnd -> ")"
    TokBracketStart -> "["
    TokBracketEnd -> "]"
    TokBraceStart -> "{"
    TokBraceEnd -> "}"
    TokEQ -> "="
    TokNEQ -> "/="
    TokGT -> ">"
    TokGTE -> ">="
    TokLT -> "<"
    TokLTE -> "<="
    TokImpl -> "=>"
    TokConj -> "&&"
    TokDisj -> "||"
    TokNeg -> "~"
    TokAdd -> "+"
    TokSub -> "-"
    TokMul -> "*"
    TokDiv -> "/"
    TokMod -> "%"
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
  <|> TokArray      <$ text "array"
  <|> TokOf         <$ text "of"
  <|> TokRange      <$ text ".."

  <|> TokGuardBar   <$ text "|"
  <|> TokArrow      <$ text "->"


  -- delimiters
  <|> TokComma        <$ text ","
  <|> TokSemi         <$ text ":"
  <|> TokAssign       <$ text ":="
  <|> TokSpecStart    <$ text "{!"
  <|> TokSpecEnd      <$ text "!}"
  <|> TokParenStart   <$ text "("
  <|> TokParenEnd     <$ text ")"
  <|> TokBracketStart <$ text "["
  <|> TokBracketEnd   <$ text "]"
  <|> TokBraceStart   <$ text "{"
  <|> TokBraceEnd     <$ text "}"

  -- literals
  <|> TokEQ           <$ text "="
  <|> TokNEQ          <$ text "/="
  <|> TokGT           <$ text ">"
  <|> TokGTE          <$ text ">="
  <|> TokLT           <$ text "<"
  <|> TokLTE          <$ text "<="
  <|> TokImpl         <$ text "=>"
  <|> TokConj         <$ text "&&"
  <|> TokDisj         <$ text "||"
  <|> TokNeg          <$ text "~"

  <|> TokAdd          <$ text "+"
  <|> TokSub          <$ text "-"
  <|> TokMul          <$ text "*"
  <|> TokDiv          <$ text "/"
  <|> TokMod          <$ text "%"

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

type LexicalError = Pos

scan :: FilePath -> Text -> Either LexicalError TokStream
scan filepath = filterError . runLexer lexer filepath . Text.unpack
  where
    filterError :: TokStream -> Either LexicalError TokStream
    filterError TsEof = Right TsEof
    filterError (TsError (Lex.LexicalError pos)) = Left pos
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
