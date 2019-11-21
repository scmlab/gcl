{-# LANGUAGE OverloadedStrings #-}

module Syntax.LexerNew where

import Language.Lexer.Applicative
import Text.Regex.Applicative
import Data.Char hiding (Space)
import Data.Text (Text, pack)
import Data.Loc (L)

data Tok
    = TokNewline
    | TokTab
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

    | TokHole

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

    deriving (Eq, Ord, Show)

tokRE :: RE Char Tok
tokRE
   =  TokSkip       <$ "skip"
  <|> TokAbort      <$ "abort"
  <|> TokDo         <$ "do"
  <|> TokOd         <$ "od"
  <|> TokIf         <$ "if"
  <|> TokFi         <$ "fi"
  <|> TokBnd        <$ "bnd"

  <|> TokHole       <$ "?"

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

  <|> TokUpperName    <$> upperNameRE
  <|> TokLowerName    <$> lowerNameRE
  <|> TokInt          <$> intRE
  <|> TokTrue         <$  "False"
  <|> TokFalse        <$  "True"

-- starts with lowercase alphabets
lowerNameRE :: RE Char Text
lowerNameRE = fmap pack $ (:) <$> psym isLower <*> many (psym (\c -> isAlphaNum c || c == '_' || c == '\''))

-- starts with uppercase alphabets
upperNameRE :: RE Char Text
upperNameRE = fmap pack $ (:) <$> psym isUpper <*> many (psym (\c -> isAlphaNum c || c == '_' || c == '\''))

intRE :: RE Char Int
intRE = read <$> some (psym isDigit)

whitespaceRE :: RE Char Tok
whitespaceRE = matchWhen isSpace TokWhitespace
-- whitespaceRE = matchWhen (\c -> c != '\n' && c != '\r') TokenWhitespace
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
  , whitespace  (longest whitespaceRE)
  , whitespace  (longestShortest commentStartRE commentEndRE)
  ]

type TStream = TokenStream (L Tok)

-- run :: Text -> []
-- runLexer

-- -- import Syntax.Parser.Type
--
-- import Control.Monad.State
-- import Control.Monad.Except
-- import Data.Char
-- import Data.Loc
-- import Data.Text (Text, pack)
-- import Language.Lexer.Applicative
-- import Text.Regex.Applicative
--
-- matchWhen :: (s -> Bool) -> a -> RE s a
-- matchWhen p symbol = msym (\t -> if p t then Just symbol else Nothing)
--
-- --------------------------------------------------------------------------------
--
--
-- tokenRE :: RE Char Token
-- tokenRE
--   =   TokenDefn         <$ "="
--   <|> TokHasType      <$ ":"
--   -- type
--   -- <|> TokVarPrefix    <$ "$"
--   <|> TokDual         <$ "^"
--   <|> TokTimes        <$ "*"
--   <|> TokPar          <$ "%"
--   <|> TokPlus         <$ "+"
--   <|> TokWith         <$ "&"
--   <|> TokAcc          <$ "!"
--   <|> TokReq          <$ "?"
--   <|> TokExists       <$ "exists"
--   <|> TokForall       <$ "forall"
--   <|> TokOne          <$ "1"
--   <|> TokBot          <$ "Bot"
--   <|> TokZero         <$ "0"
--   <|> TokTop          <$ "Top"
--   <|> TokSessionStart <$ "{"
--   <|> TokSessionEnd   <$ "}"
--   <|> TokSessionSep   <$ ";"
--   -- term
--   <|> TokLink         <$ "<->"
--   <|> TokScope        <$ "\\"
--   <|> TokSeq          <$ "."
--   <|> TokComp         <$ "|"
--   <|> TokParenStart   <$ "("
--   <|> TokParenEnd     <$ ")"
--   <|> TokBracketStart <$ "["
--   <|> TokBracketEnd   <$ "]"
--   <|> TokSelectL      <$ "[inl]"
--   <|> TokSelectR      <$ "[inr]"
--   <|> TokCase         <$ "case"
--   <|> TokCaseSep      <$ ","
--   <|> TokEmptyOutput  <$ "[]"
--   <|> TokEnd          <$ "end"
--   <|> TokEmptyInput   <$ "()"
--   <|> TokEmptyChoice  <$ "case()"
--   <|> TokTypeName     <$> typeName
--   <|> TokTermName     <$> termName
--   -- <|> TokInt          <$> intRE
--
-- (+++) :: RE Char String -> RE Char String -> RE Char String
-- (+++) = liftA2 (++)
--
-- -- starts with lowercase alphabets
-- termName :: RE Char Text
-- termName = fmap pack $ (:) <$> psym isLower <*> many (psym (\c -> isAlphaNum c || c == '_' || c == '\''))
--
-- -- starts with uppercase alphabets
-- typeName :: RE Char Text
-- typeName = fmap pack $ (:) <$> psym isUpper <*> many (psym (\c -> isAlphaNum c || c == '_' || c == '\''))
--
-- -- all uppercase alphabets
-- -- labelRE :: RE Char Text
-- -- labelRE = fmap pack $ (:) <$> psym isUpper <*> many (psym (\c -> isUpper c || isDigit c || c == '_' || c == '\''))
--
-- -- stringRE :: RE Char Text
-- -- stringRE = fmap pack $ string "\"" +++ firstPart +++ secondPart +++ string "\""
-- --   where
-- --     -- zero or more characters other than \ and "
-- --     firstPart :: RE Char String
-- --     firstPart = many (psym (\c -> c /= '\\' && c /= '\"'))
-- --
-- --     -- zero or more sequences of (`secondPart1` and then `firstPart`)
-- --     secondPart :: RE Char String
-- --     secondPart = reFoldl Greedy (++) "" (secondPart1 +++ firstPart)
-- --
-- --     -- a backslash followed with any character but a newline
-- --     secondPart1 :: RE Char String
-- --     secondPart1 = string "\\" +++ msym (\c -> if c == '\n' then Nothing else Just [c])
--
-- -- intRE :: RE Char Int
-- -- intRE = read <$> some (psym isDigit)
--
-- whitespaceRE :: RE Char Token
-- whitespaceRE = matchWhen isSpace TokenWhitespace
--
-- commentStartRE :: RE Char String
-- commentStartRE = string "--"
--
-- commentEndRE :: String -> RE Char Token
-- commentEndRE pref = TokenComment <$> fmap pack (pure pref +++ many anySym +++ string "\n")
--
-- lexer :: Lexer Token
-- lexer = mconcat
--   [ token       (longest tokenRE)
--   , whitespace  (longest whitespaceRE)
--   , whitespace  (longestShortest commentStartRE commentEndRE)
--   ]
--
-- --------------------------------------------------------------------------------
--
-- scan :: (Token -> Parser a) -> Parser a
-- scan f = scanNext >>= f
--
-- scanNext :: Parser Token
-- scanNext = do
--   result <- gets tokenStream
--   oldLoc <- gets lookaheadLoc
--   src    <- gets rawSource
--   case result of
--     TsToken (L newLoc tok) stream -> do
--       put $ ParserState oldLoc newLoc stream src
--       return tok
--     TsEof -> do
--       modify $ \st -> st { currentLoc = oldLoc , lookaheadLoc = oldLoc}
--       return TokenEOF
--     TsError (LexicalError pos) -> do
--       throwError $ Lexical src pos
