{-# LANGUAGE OverloadedStrings #-}

module Syntax.Parser.Lexer where

import Syntax.Parser.TokenStream

import Data.List.NonEmpty (NonEmpty(..))

import Language.Lexer.Applicative
import Text.Regex.Applicative
import Data.Char hiding (Space)
import Data.Text (Text, pack, unpack)
import Data.Loc

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
    TokComment s -> "-- " ++ unpack s
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
    TokSemi -> ";"
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
    TokUpperName s -> unpack s
    TokLowerName s -> unpack s
    TokInt i -> show i
    TokTrue -> "True"
    TokFalse -> "False"

tokRE :: RE Char Tok
tokRE
   =
      TokNewline    <$ "\n"

  <|> TokSkip       <$ "skip"
  <|> TokAbort      <$ "abort"
  <|> TokDo         <$ "do"
  <|> TokOd         <$ "od"
  <|> TokIf         <$ "if"
  <|> TokFi         <$ "fi"
  <|> TokBnd        <$ "bnd"

  <|> TokQM         <$ "?"

  <|> TokCon        <$ "con"
  <|> TokVar        <$ "var"

  <|> TokGuardBar   <$ "|"
  <|> TokGuardArr   <$ "->"

  -- delimiters
  <|> TokComma        <$ ","
  <|> TokSemi         <$ ":"
  <|> TokAssign       <$ ":="
  <|> TokSpecStart    <$ "{!"
  <|> TokSpecEnd      <$ "!}"
  <|> TokParenStart   <$ "("
  <|> TokParenEnd     <$ ")"
  <|> TokBraceStart   <$ "{"
  <|> TokBraceEnd     <$ "}"

  -- literals
  <|> TokEQ           <$  "="
  <|> TokGT           <$  ">"
  <|> TokGTE          <$  ">="
  <|> TokLT           <$  "<"
  <|> TokLTE          <$  "<="
  <|> TokImpl         <$  "=>"
  <|> TokConj         <$  "&&"
  <|> TokDisj         <$  "||"
  <|> TokNeg          <$  "^"

  <|> TokTrue         <$  "True"
  <|> TokFalse        <$  "False"

  <|> TokUpperName    <$> upperNameRE
  <|> TokLowerName    <$> lowerNameRE
  <|> TokInt          <$> intRE

-- starts with lowercase alphabets
lowerNameRE :: RE Char Text
lowerNameRE = fmap pack $ (:) <$> psym isLower <*> many (psym (\c -> isAlphaNum c || c == '_' || c == '\''))

-- starts with uppercase alphabets
upperNameRE :: RE Char Text
upperNameRE = fmap pack $ (:) <$> psym isUpper <*> many (psym (\c -> isAlphaNum c || c == '_' || c == '\''))

intRE :: RE Char Int
intRE = read <$> some (psym isDigit)

whitespaceButNewlineRE :: RE Char Tok
-- whitespaceRE = matchWhen isSpace TokWhitespace
whitespaceButNewlineRE = matchWhen (\c -> isSpace c && c /= '\n' && c /= '\r') TokWhitespace
  where
    matchWhen :: (s -> Bool) -> a -> RE s a
    matchWhen p symbol = msym (\t -> if p t then Just symbol else Nothing)


commentStartRE :: RE Char String
commentStartRE = string "--"

commentEndRE :: String -> RE Char Tok
commentEndRE pref = TokComment <$> fmap pack (pure pref +++ many anySym +++ string "\n")
  where
    (+++) :: RE Char String -> RE Char String -> RE Char String
    (+++) = liftA2 (++)

lexer :: Lexer Tok
lexer = mconcat
  [ token       (longest tokRE)
  , whitespace  (longest whitespaceButNewlineRE)
  , whitespace  (longestShortest commentStartRE commentEndRE)
  ]

scan :: FilePath -> Text -> TokStream
scan filepath raw = runLexer lexer filepath (unpack raw)

type TokStream = TokenStream (L Tok)

-- instance Streamable Tok where
--   showNonEmptyTokens (x :| xs) = show x ++ concat (map (show . unLoc) xs)
--     where
--       front :: String
--       front = case lines (show x) of
--                 [] -> ""
--                 xs -> last xs
