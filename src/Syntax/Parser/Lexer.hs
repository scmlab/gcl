{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Syntax.Parser.Lexer where

import Control.Applicative.Combinators (choice, many, skipMany, some, (<|>))
import Control.Monad (void)
import Data.Char (isAlpha, isAlphaNum, isSpace, isSymbol)
import Data.Loc (Loc (..), Located (..), Pos, (<-->))
import Data.Loc.Range (Range (Range), Ranged (rangeOf))
import Data.Proxy (Proxy (Proxy))
import Data.Text (Text)
import Data.Void (Void)
import Syntax.Common
import Syntax.Concrete (Lit (..), Token (..))
import Syntax.Parser.Token
import Syntax.Parser.Util
import Text.Megaparsec (MonadParsec (notFollowedBy, tokens, try), Parsec, Stream (tokensToChunk), getOffset, getSourcePos, satisfy, setOffset, (<?>))
import Text.Megaparsec.Char (alphaNumChar, char, lowerChar, space1, string, upperChar)
import qualified Text.Megaparsec.Char.Lexer as Lex
import Data.Bifunctor (second)
import qualified Data.Text as Text

type Lexer = Parsec Void Text

type LexerF = ParseFunc Lexer

spaceNotNewline :: Lexer Char
spaceNotNewline = satisfy (\t -> isSpace t && t /= '\n' && t /= '\r' && t /= '\f' && t /= '\v')

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
lexeme p = (↑) (\sc' -> Lex.lexeme (try sc' <|> sc) . getRange $ p)

symbol :: Text -> LexerF (Token a)
symbol t = do
  (_, Range l r) <- lexeme . string $ t
  return $ Token l r

------------------------------------------
-- lexical token
------------------------------------------
lexSkip :: LexerF (Token tokSkip)
lexSkip = symbol tokSkip

lexAbort :: LexerF (Token tokAbort)
lexAbort = symbol tokAbort

lexDo :: LexerF (Token tokDo)
lexDo = symbol tokDo

lexOd :: LexerF (Token tokOd)
lexOd = symbol tokOd

lexIf :: LexerF (Token tokIf)
lexIf = symbol tokIf

lexFi :: LexerF (Token tokFi)
lexFi = symbol tokFi

lexBnd :: LexerF (Token tokBnd)
lexBnd = symbol tokBnd

lexQM :: LexerF (Token tokQM)
lexQM = symbol tokQM

lexCon :: LexerF (Token tokCon)
lexCon = symbol tokCon

lexVar :: LexerF (Token tokVar)
lexVar = symbol tokVar

lexLet :: LexerF (Token tokLet)
lexLet = symbol tokLet

lexArray :: LexerF (Token tokArray)
lexArray = symbol tokArray

lexOf :: LexerF (Token tokOf)
lexOf = symbol tokOf

lexRange :: LexerF (Token tokRange)
lexRange = symbol tokRange

lexGuardBar :: LexerF (Token tokGuardBar)
lexGuardBar = symbol tokGuardBar

lexArrow :: LexerF (Either (Token tokArrow) (Token tokArrowU))
lexArrow = choice [Left <$> symbol tokArrow, Right <$> symbol tokArrowU]

------------------------------------------
-- delimiters
------------------------------------------

lexSpace :: LexerF (Token tokSpace)
lexSpace = symbol tokSpace

lexComma :: LexerF (Token tokComma)
lexComma = symbol tokComma

lexColon :: LexerF (Token tokColon)
lexColon = symbol tokColon

lexSemi :: LexerF (Token tokSemi)
lexSemi = symbol tokSemi

lexAssign :: LexerF (Token tokAssign)
lexAssign = symbol tokAssign

lexSpecStart :: LexerF (Token tokSpecStart)
lexSpecStart = symbol tokSpecStart

lexSpecEnd :: LexerF (Token tokSpecEnd)
lexSpecEnd = symbol tokSpecEnd

lexParenStart :: LexerF (Token tokParenStart)
lexParenStart = symbol tokParenStart

lexParenEnd :: LexerF (Token tokParenEnd)
lexParenEnd = symbol tokParenEnd

lexBracketStart :: LexerF (Token tokBracketStart)
lexBracketStart = symbol tokBracketStart

lexBracketEnd :: LexerF (Token tokBracketEnd)
lexBracketEnd = symbol tokBracketEnd

lexBraceStart :: LexerF (Token tokBraceStart)
lexBraceStart = symbol tokBraceStart

lexBraceEnd :: LexerF (Token tokBraceEnd)
lexBraceEnd = symbol tokBraceEnd

lexQuantStarts :: LexerF (Either (Token tokQuantStarts) (Token tokQuantStartU))
lexQuantStarts = choice [Left <$> symbol tokQuantStarts, Right <$> symbol tokQuantStartU]

lexQuantEnds :: LexerF (Either (Token tokQuantEnds) (Token tokQuantEndU))
lexQuantEnds = choice [Left <$> symbol tokQuantEnds, Right <$> symbol tokQuantEndU]

lexProofStart :: LexerF (Token tokProofStart)
lexProofStart = symbol tokProofStart

lexProofEnd :: LexerF (Token tokProofEnd)
lexProofEnd = symbol tokProofEnd


-- expects someting like #3ab4bd33e7a32de7
-- the range coverts the whole thing, but the text includes only the alphanum part (that is, without prefix '#')  
lexProofAnchor :: LexerF (Text, Range)
lexProofAnchor = lexeme $ do 
  _ <- char '#' 
  hash <- many . satisfy $ isAlphaNum
  return $ Text.pack hash 

lexBackSlash :: LexerF (Token tokBackSlash)
lexBackSlash = symbol tokBackSlash

lexDeclStart :: LexerF (Token tokDeclStart)
lexDeclStart = symbol tokDeclStart

lexDeclEnd :: LexerF (Token tokDeclEnd)
lexDeclEnd = symbol tokDeclEnd

------------------------------------------
-- Operators
------------------------------------------

lexEQProp :: LexerF ChainOp
lexEQProp = EQProp . locOf <$> symbol tokEQProp

lexEQPropU :: LexerF ChainOp
lexEQPropU = EQPropU . locOf <$> symbol tokEQPropU

lexEQ' :: LexerF (Token tokEQ)
lexEQ' = symbol tokEQ

lexEQ :: LexerF ChainOp
lexEQ = Syntax.Common.EQ . locOf <$> symbol tokEQ

lexNEQ :: LexerF ChainOp
lexNEQ = NEQ . locOf <$> symbol tokNEQ

lexNEQU :: LexerF ChainOp
lexNEQU = NEQU . locOf <$> symbol tokNEQU

lexGT :: LexerF ChainOp
lexGT = Syntax.Common.GT . locOf <$> symbol tokGT

lexGTE :: LexerF ChainOp
lexGTE = GTE . locOf <$> symbol tokGTE

lexGTEU :: LexerF ChainOp
lexGTEU = GTEU . locOf <$> symbol tokGTEU

lexLT :: LexerF ChainOp
lexLT = Syntax.Common.LT . locOf <$> symbol tokLT

lexLTE :: LexerF ChainOp
lexLTE = LTE . locOf <$> symbol tokLTE

lexLTEU :: LexerF ChainOp
lexLTEU = LTEU . locOf <$> symbol tokLTEU

lexImpl :: LexerF ArithOp
lexImpl = Implies . locOf <$> symbol tokImpl

lexImplU :: LexerF ArithOp
lexImplU = ImpliesU . locOf <$> symbol tokImplU

lexConj :: LexerF ArithOp
lexConj = Conj . locOf <$> symbol tokConj

lexConjU :: LexerF ArithOp
lexConjU = ConjU . locOf <$> symbol tokConjU

lexDisj :: LexerF ArithOp
lexDisj = Disj . locOf <$> symbol tokDisj

lexDisjU :: LexerF ArithOp
lexDisjU = DisjU . locOf <$> symbol tokDisjU

lexNeg :: LexerF ArithOp
lexNeg = Neg . locOf <$> symbol tokNeg

lexNegU :: LexerF ArithOp
lexNegU = NegU . locOf <$> symbol tokNegU

lexAdd :: LexerF ArithOp
lexAdd = Add . locOf <$> symbol tokAdd

lexSub :: LexerF ArithOp
lexSub = Sub . locOf <$> symbol tokSub

lexMul :: LexerF ArithOp
lexMul = Mul . locOf <$> symbol tokMul

lexDiv :: LexerF ArithOp
lexDiv = Div . locOf <$> symbol tokDiv

lexMod :: LexerF ArithOp
lexMod = Mod . locOf <$> symbol tokMod

lexMax :: LexerF ArithOp  
lexMax = Max . locOf <$> symbol tokMax

lexMin :: LexerF ArithOp  
lexMin = Min . locOf <$> symbol tokMin

lexExp :: LexerF ArithOp  
lexExp = Exp . locOf <$> symbol tokExp

lexSum :: LexerF ArithOp 
lexSum = Add . locOf <$> symbol tokSum

lexPi :: LexerF ArithOp
lexPi = Mul . locOf <$> symbol tokPi

lexForall :: LexerF ArithOp
lexForall = Conj . locOf <$> symbol tokForall

lexExists :: LexerF ArithOp
lexExists = Disj . locOf <$> symbol tokExists

lexHash :: LexerF ArithOp 
lexHash = Hash . locOf <$> symbol tokHash

lexChainOps :: LexerF ChainOp 
lexChainOps = 
  choice 
    [ lexEQProp, 
      lexEQPropU,
      lexEQ, 
      lexNEQ, 
      lexNEQU, 
      lexGT, 
      lexGTE, 
      lexGTEU,
      lexLT, 
      lexLTE, 
      lexLTEU
    ]

lexArithOps :: LexerF ArithOp 
lexArithOps = 
  choice 
    [ lexImpl, 
      lexImplU, 
      lexConj, 
      lexConjU, 
      lexDisj, 
      lexDisjU,
      lexNeg, 
      lexNegU, 
      lexAdd, 
      lexSub, 
      lexMul, 
      lexDiv, 
      lexMod,
      lexMax, 
      lexMin,
      lexExp,
      lexSum, 
      lexPi,
      lexForall, 
      lexExists, 
      lexHash
     
    ]

lexOps :: LexerF Op
lexOps =
  choice
    [ ChainOp <$> lexChainOps,
      ArithOp <$> lexArithOps
    ]
    <?> "operators"

------------------------------------------
-- literals
------------------------------------------

lexUpper :: LexerF (Text, Range)
lexUpper = lexeme . try . withPredicate notUpperKeywords $ do
  x <- upperChar
  xs <- many . satisfy $ (\c -> isAlphaNum c || c == '_' || c == '\'')
  return $ tokensToChunk (Proxy :: Proxy Text) (x : xs)

lexLower :: LexerF (Text, Range)
lexLower = lexeme . try . withPredicate notLowerKeywords $ do
  x <- lowerChar
  xs <- many . satisfy $ (\c -> isAlphaNum c || c == '_' || c == '\'')
  return $ tokensToChunk (Proxy :: Proxy Text) (x : xs)

lexText :: LexerF (Text, Range)
lexText = lexeme . try . withPredicate (\t -> notUpperKeywords t && notLowerKeywords t) $ do
  x <- satisfy isAlpha
  xs <- many . satisfy $ (\c -> isAlphaNum c || c == '_' || c == '\'')
  return $ tokensToChunk (Proxy :: Proxy Text) (x : xs)

lexTrue :: LexerF Lit
lexTrue = LitBool True . locOf <$> symbol tokTrue

lexFalse :: LexerF Lit
lexFalse = LitBool False . locOf <$> symbol tokFalse

lexInt :: LexerF Lit
lexInt = uncurry LitInt . second locOf <$> lexeme Lex.decimal

lexChar :: LexerF Lit
lexChar = uncurry LitChar . second locOf <$> lexeme (char '\'' *> Lex.charLiteral <* char '\'')

lexLits :: LexerF Lit
lexLits = choice [lexTrue, lexFalse, lexInt, lexChar] <?> "literals"

lexTypeInt :: LexerF (Token tokTypeInt)
lexTypeInt = symbol tokTypeInt

lexTypeBool :: LexerF (Token tokTypeBool)
lexTypeBool = symbol tokTypeBool

lexTypeChar :: LexerF (Token tokTypeChar)
lexTypeChar = symbol tokTypeChar

------------------------------------------
-- helper combinators
------------------------------------------

getLoc :: Lexer a -> Lexer (a, Loc)
getLoc m = do
  start <- getCurPos
  x <- m
  end <- getEndPos
  return (x, Loc start end)

getRange :: Lexer a -> Lexer (a, Range)
getRange m = do
  start <- getCurPos
  x <- m
  end <- getEndPos
  return (x, Range start end)

withLoc :: Located a => Lexer a -> Lexer (a, Loc)
withLoc p = do
  x <- p
  loc <- locOf <$> p
  return (x, loc)

withRange :: Ranged a => Lexer a -> Lexer (a, Range)
withRange p = do
  x <- p
  range <- rangeOf <$> p
  return (x, range)

-- NOTE : make sure no space consumed after parser m
notFollowedBySymbol :: LexerF a -> LexerF a
notFollowedBySymbol m = ParseFunc (\sc' -> (↓) m (return ()) <* notFollowedBy (satisfy isSymbol) <* sc')

withPredicate :: (a -> Bool) -> Lexer a -> Lexer a
withPredicate f p = do
  o <- getOffset
  x <- p
  if f x
    then return x
    else do
      setOffset o
      fail "using keyword as variable name"
