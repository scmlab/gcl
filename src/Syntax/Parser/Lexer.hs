{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Syntax.Parser.Lexer where

import           Control.Applicative.Combinators
                                                ( choice
                                                , many
                                                , skipMany
                                                , some
                                                , (<|>)
                                                )
import           Control.Monad                  ( void )
import           Data.Char                      ( isAlpha
                                                , isAlphaNum
                                                , isSpace
                                                , isSymbol
                                                )
import           Data.Loc                       ( Loc(..)
                                                , Located(..)
                                                , Pos
                                                , (<-->)
                                                )
import           Data.Loc.Range                 ( Range(Range)
                                                , Ranged(rangeOf)
                                                )
import           Data.Proxy                     ( Proxy(Proxy) )
import           Data.Text                      ( Text )
import           Data.Void                      ( Void )
import           Syntax.Common
import           Syntax.Concrete                ( Lit(..)
                                                , Token(..)
                                                )
import           Syntax.Parser.Token
import           Syntax.Parser.Util
import           Text.Megaparsec                ( MonadParsec
                                                  ( notFollowedBy
                                                  , tokens
                                                  , try
                                                  )
                                                , Parsec
                                                , Stream(tokensToChunk)
                                                , getOffset
                                                , getSourcePos
                                                , satisfy
                                                , setOffset
                                                , (<?>)
                                                )
import           Text.Megaparsec.Char           ( alphaNumChar
                                                , char
                                                , lowerChar
                                                , space1
                                                , string
                                                , upperChar
                                                )
import qualified Text.Megaparsec.Char.Lexer    as Lex
import           Data.Bifunctor                 ( second )
import qualified Data.Text                     as Text

type Lexer = Parsec Void Text

type LexerF = ParseFunc Lexer

spaceNotNewline :: Lexer Char
spaceNotNewline =
  satisfy (\t -> isSpace t && t /= '\n' && t /= '\r' && t /= '\f' && t /= '\v')

scn :: Lexer ()
scn = Lex.space space1 skipLineComment skipBlockComment

sc :: Lexer ()
sc = Lex.space (void $ some spaceNotNewline) skipLineComment skipBlockComment

-- Note: Should consume the newline character or not ?
skipLineComment :: Lexer ()
skipLineComment = Lex.skipLineComment tokLineComment

-- NOTE: Not sure what the symbol of block comment will be
skipBlockComment :: Lexer ()
skipBlockComment = Lex.skipBlockComment tokBlockCommentStart tokBlockCommentEnd

-- wrapper of tokens

lexeme :: Lexer a -> LexerF (a, Range)
lexeme p = ParseFunc (\sc' -> Lex.lexeme (try sc' <|> sc) . getRange $ p)

symbol :: Text -> LexerF (Token a)
symbol t = do
  (_, Range l r) <- lexeme . string $ t
  return $ Token l r

------------------------------------------
-- lexical token
------------------------------------------
lexSkipF :: LexerF (Token tokSkip)
lexSkipF = symbol tokSkip

lexAbortF :: LexerF (Token tokAbort)
lexAbortF = symbol tokAbort

lexDoF :: LexerF (Token tokDo)
lexDoF = symbol tokDo

lexOdF :: LexerF (Token tokOd)
lexOdF = symbol tokOd

lexIfF :: LexerF (Token tokIf)
lexIfF = symbol tokIf

lexFiF :: LexerF (Token tokFi)
lexFiF = symbol tokFi

lexBndF :: LexerF (Token tokBnd)
lexBndF = symbol tokBnd

lexQMF :: LexerF (Token tokQM)
lexQMF = symbol tokQM

lexConF :: LexerF (Token tokCon)
lexConF = symbol tokCon

lexVarF :: LexerF (Token tokVar)
lexVarF = symbol tokVar

lexLetF :: LexerF (Token tokLet)
lexLetF = symbol tokLet

lexDataF :: LexerF (Token tokData)
lexDataF = symbol tokData

lexArrayF :: LexerF (Token tokArray)
lexArrayF = symbol tokArray

lexOfF :: LexerF (Token tokOf)
lexOfF = symbol tokOf

lexNewF :: LexerF (Token tokNew)
lexNewF = symbol tokNew

lexDisposeF :: LexerF (Token tokDispose)
lexDisposeF = symbol tokDispose

lexRangeF :: LexerF (Token tokRange)
lexRangeF = symbol tokRange

lexGuardBarF :: LexerF (Token tokGuardBar)
lexGuardBarF = symbol tokGuardBar

lexArrowF :: LexerF (Either (Token tokArrow) (Token tokArrowU))
lexArrowF = choice [Left <$> symbol tokArrow, Right <$> symbol tokArrowU]

lexStarF :: LexerF (Token tokStar)
lexStarF = symbol tokStar

lexEqualF :: LexerF (Token tokEQ)
lexEqualF = symbol tokEQ

------------------------------------------
-- delimiters
------------------------------------------

lexSpaceF :: LexerF (Token tokSpace)
lexSpaceF = symbol tokSpace

lexCommaF :: LexerF (Token tokComma)
lexCommaF = symbol tokComma

lexColonF :: LexerF (Token tokColon)
lexColonF = symbol tokColon

lexSemiF :: LexerF (Token tokSemi)
lexSemiF = symbol tokSemi

lexAssignF :: LexerF (Token tokAssign)
lexAssignF = symbol tokAssign

lexSpecStartF :: LexerF (Token tokSpecStart)
lexSpecStartF = symbol tokSpecStart

lexSpecEndF :: LexerF (Token tokSpecEnd)
lexSpecEndF = symbol tokSpecEnd

lexParenStartF :: LexerF (Token tokParenStart)
lexParenStartF = symbol tokParenStart

lexParenEndF :: LexerF (Token tokParenEnd)
lexParenEndF = symbol tokParenEnd

lexBracketStartF :: LexerF (Token tokBracketStart)
lexBracketStartF = symbol tokBracketStart

lexBracketEndF :: LexerF (Token tokBracketEnd)
lexBracketEndF = symbol tokBracketEnd

lexBraceStartF :: LexerF (Token tokBraceStart)
lexBraceStartF = symbol tokBraceStart

lexBraceEndF :: LexerF (Token tokBraceEnd)
lexBraceEndF = symbol tokBraceEnd

lexQuantStartsF
  :: LexerF (Either (Token tokQuantStarts) (Token tokQuantStartU))
lexQuantStartsF =
  choice [Left <$> symbol tokQuantStarts, Right <$> symbol tokQuantStartU]

lexQuantEndsF :: LexerF (Either (Token tokQuantEnds) (Token tokQuantEndU))
lexQuantEndsF =
  choice [Left <$> symbol tokQuantEnds, Right <$> symbol tokQuantEndU]

lexProofStartF :: LexerF (Token tokProofStart)
lexProofStartF = symbol tokProofStart

lexProofEndF :: LexerF (Token tokProofEnd)
lexProofEndF = symbol tokProofEnd


-- expects someting like #3ab4bd33e7a32de7
-- the range coverts the whole thing, but the text includes only the alphanum part (that is, without prefix '#')
lexProofAnchorF :: LexerF (Text, Range)
lexProofAnchorF = lexeme $ do
  _    <- char '#'
  hash <- many . satisfy $ isAlphaNum
  return $ Text.pack hash

lexBackSlashF :: LexerF (Token tokBackSlash)
lexBackSlashF = symbol tokBackSlash

lexDeclStartF :: LexerF (Token tokDeclStart)
lexDeclStartF = symbol tokDeclStart

lexDeclEndF :: LexerF (Token tokDeclEnd)
lexDeclEndF = symbol tokDeclEnd

lexBlockStartF :: LexerF (Token tokBlockStart)
lexBlockStartF = symbol tokBlockStart

lexBlockEndF :: LexerF (Token tokBlockEnd)
lexBlockEndF = symbol tokBlockEnd

------------------------------------------
-- Operators
------------------------------------------

lexEQPropF :: LexerF ChainOp
lexEQPropF = EQProp . locOf <$> symbol tokEQProp

lexEQPropUF :: LexerF ChainOp
lexEQPropUF = EQPropU . locOf <$> symbol tokEQPropU


lexEQF :: LexerF ChainOp
lexEQF = Syntax.Common.EQ . locOf <$> symbol tokEQ

lexNEQF :: LexerF ChainOp
lexNEQF = NEQ . locOf <$> symbol tokNEQ

lexNEQUF :: LexerF ChainOp
lexNEQUF = NEQU . locOf <$> symbol tokNEQU

lexGTF :: LexerF ChainOp
lexGTF = Syntax.Common.GT . locOf <$> symbol tokGT

lexGTEF :: LexerF ChainOp
lexGTEF = GTE . locOf <$> symbol tokGTE

lexGTEUF :: LexerF ChainOp
lexGTEUF = GTEU . locOf <$> symbol tokGTEU

lexLTF :: LexerF ChainOp
lexLTF = Syntax.Common.LT . locOf <$> symbol tokLT

lexLTEF :: LexerF ChainOp
lexLTEF = LTE . locOf <$> symbol tokLTE

lexLTEUF :: LexerF ChainOp
lexLTEUF = LTEU . locOf <$> symbol tokLTEU

lexImplF :: LexerF ArithOp
lexImplF = Implies . locOf <$> symbol tokImpl

lexImplUF :: LexerF ArithOp
lexImplUF = ImpliesU . locOf <$> symbol tokImplU

lexConjF :: LexerF ArithOp
lexConjF = Conj . locOf <$> symbol tokConj

lexConjUF :: LexerF ArithOp
lexConjUF = ConjU . locOf <$> symbol tokConjU

lexDisjF :: LexerF ArithOp
lexDisjF = Disj . locOf <$> symbol tokDisj

lexDisjUF :: LexerF ArithOp
lexDisjUF = DisjU . locOf <$> symbol tokDisjU

lexNegF :: LexerF ArithOp
lexNegF = Neg . locOf <$> symbol tokNeg

lexNegUF :: LexerF ArithOp
lexNegUF = NegU . locOf <$> symbol tokNegU

lexAddF :: LexerF ArithOp
lexAddF = Add . locOf <$> symbol tokAdd

lexSubF :: LexerF ArithOp
lexSubF = Sub . locOf <$> symbol tokSub

lexMulF :: LexerF ArithOp
lexMulF = Mul . locOf <$> symbol tokMul

lexDivF :: LexerF ArithOp
lexDivF = Div . locOf <$> symbol tokDiv

lexModF :: LexerF ArithOp
lexModF = Mod . locOf <$> symbol tokMod

lexMaxF :: LexerF ArithOp
lexMaxF = Max . locOf <$> symbol tokMax

lexMinF :: LexerF ArithOp
lexMinF = Min . locOf <$> symbol tokMin

lexExpF :: LexerF ArithOp
lexExpF = Exp . locOf <$> symbol tokExp

lexSumF :: LexerF ArithOp
lexSumF = Add . locOf <$> symbol tokSum

lexPiF :: LexerF ArithOp
lexPiF = Mul . locOf <$> symbol tokPi

lexForallF :: LexerF ArithOp
lexForallF = Conj . locOf <$> symbol tokForall

lexExistsF :: LexerF ArithOp
lexExistsF = Disj . locOf <$> symbol tokExists

lexHashF :: LexerF ArithOp
lexHashF = Hash . locOf <$> symbol tokHash

lexChainOpsF :: LexerF ChainOp
lexChainOpsF = choice
  [ lexEQPropF
  , lexEQPropUF
  , lexEQF
  , lexNEQF
  , lexNEQUF
  , lexGTF
  , lexGTEF
  , lexGTEUF
  , lexLTF
  , lexLTEF
  , lexLTEUF
  ]

lexArithOpsF :: LexerF ArithOp
lexArithOpsF = choice
  [ lexImplF
  , lexImplUF
  , lexConjF
  , lexConjUF
  , lexDisjF
  , lexDisjUF
  , lexNegF
  , lexNegUF
  , lexAddF
  , lexSubF
  , lexMulF
  , lexDivF
  , lexModF
  , lexMaxF
  , lexMinF
  , lexExpF
  , lexSumF
  , lexPiF
  , lexForallF
  , lexExistsF
  , lexHashF
  ]

lexOpsF :: LexerF Op
lexOpsF =
  choice [ChainOp <$> lexChainOpsF, ArithOp <$> lexArithOpsF] <?> "operators"

------------------------------------------
-- literals
------------------------------------------

-- name start with uppercase letter which is not a keyword
lexUpperNameF :: LexerF Name
lexUpperNameF =
  lexTextToNameF
    .   lexeme
    .   try
    .   withPredicate notUpperKeywords
    $   upperChar
    >>= lexTextWithHd

-- name start with lowercase letter which is not a keyword
lexLowerNameF :: LexerF Name
lexLowerNameF =
  lexTextToNameF
    .   lexeme
    .   try
    .   withPredicate notLowerKeywords
    $   lowerChar
    >>= lexTextWithHd

-- name which is not a keyword
lexNameF :: LexerF Name
lexNameF =
  lexTextToNameF
    .   lexeme
    .   try
    .   withPredicate (\t -> notLowerKeywords t && notUpperKeywords t)
    $   satisfy isAlpha
    >>= lexTextWithHd

-- name with no restriction
lexAnyNameF :: LexerF Name
lexAnyNameF = lexTextToNameF . lexeme . try $ satisfy isAlpha >>= lexTextWithHd

lexTextToNameF :: LexerF (Text, Range) -> LexerF Name
lexTextToNameF l = uncurry Name . second locOf <$> l

lexTextWithHd :: Char -> Lexer Text
lexTextWithHd x = do
  xs <- many . satisfy $ (\c -> isAlphaNum c || c == '_' || c == '\'')
  return $ tokensToChunk (Proxy :: Proxy Text) (x : xs)

lexTrueF :: LexerF Lit
lexTrueF = LitBool True . rangeOf <$> symbol tokTrue

lexFalseF :: LexerF Lit
lexFalseF = LitBool False . rangeOf <$> symbol tokFalse
lexIntF :: LexerF Lit
lexIntF = uncurry LitInt . second rangeOf <$> lexeme Lex.decimal

lexCharF :: LexerF Lit
lexCharF = uncurry LitChar . second rangeOf <$> lexeme
  (char '\'' *> Lex.charLiteral <* char '\'')

lexLitsF :: LexerF Lit
lexLitsF = choice [lexTrueF, lexFalseF, lexIntF, lexCharF] <?> "literals"

lexTypeIntF :: LexerF (Token tokTypeInt)
lexTypeIntF = symbol tokTypeInt

lexTypeBoolF :: LexerF (Token tokTypeBool)
lexTypeBoolF = symbol tokTypeBool

lexTypeCharF :: LexerF (Token tokTypeChar)
lexTypeCharF = symbol tokTypeChar

lexUnderscore :: LexerF (Token lexUnderscore)
lexUnderscore = symbol tokUnderscore

------------------------------------------
-- helper combinators
------------------------------------------

getLoc :: Lexer a -> Lexer (a, Loc)
getLoc m = do
  start <- getCurPos
  x     <- m
  end   <- getEndPos
  return (x, Loc start end)

getRange :: Lexer a -> Lexer (a, Range)
getRange m = do
  start <- getCurPos
  x     <- m
  end   <- getEndPos
  return (x, Range start end)

withLoc :: Located a => Lexer a -> Lexer (a, Loc)
withLoc p = do
  x   <- p
  loc <- locOf <$> p
  return (x, loc)

withRange :: Ranged a => Lexer a -> Lexer (a, Range)
withRange p = do
  x     <- p
  range <- rangeOf <$> p
  return (x, range)

-- NOTE : make sure no space consumed after parser m
notFollowedBySymbolF :: LexerF a -> LexerF a
notFollowedBySymbolF m = ParseFunc
  (\sc' -> unParseFunc m (return ()) <* notFollowedBy (satisfy isSymbol) <* sc')

withPredicate :: (a -> Bool) -> Lexer a -> Lexer a
withPredicate f p = do
  o <- getOffset
  x <- p
  if f x
    then return x
    else do
      setOffset o
      fail "using keyword as variable name"
