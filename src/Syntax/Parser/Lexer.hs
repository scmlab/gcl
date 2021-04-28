{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}

{-# OPTIONS_GHC -Wno-unused-imports #-}
module Syntax.Parser.Lexer where

import Control.Monad (void)
import Control.Applicative.Combinators ((<|>), choice, some, many, skipMany)
import Data.Void (Void)
import Data.Proxy (Proxy(Proxy))
import Data.Char (isSymbol, isSpace, isAlphaNum)
import Data.Text (Text)
import Data.Loc (Located(..), Loc(..), (<-->), Pos)
import Text.Megaparsec (setOffset, getOffset, MonadParsec(try, notFollowedBy, tokens), getSourcePos, Stream(tokensToChunk), satisfy, (<?>), Parsec )
import Text.Megaparsec.Char (string, alphaNumChar, lowerChar, char, upperChar, space1)
import qualified Text.Megaparsec.Char.Lexer as Lex
import Syntax.Concrete (Lit(..), Token (..))
import Syntax.Common
import Syntax.Parser.Token
import Syntax.Parser.Util

type LexicalError = Pos
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
lexeme :: Lexer a -> LexerF (a, Loc)
lexeme p = (↑) (\sc' -> Lex.lexeme (try sc' <|> sc) . getLoc $ p)

symbol :: Text -> LexerF (Token a)
symbol t = do
  (_, loc) <- lexeme . string $ t
  case loc of
    NoLoc -> error "NoLoc when parsing token"
    Loc l r -> return $ Token l r

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

lexBackSlash :: LexerF (Token tokBackSlash)
lexBackSlash = symbol tokBackSlash 

lexDeclStart :: LexerF (Token tokDeclStart)
lexDeclStart = symbol tokDeclStart 

lexDeclEnd :: LexerF (Token tokDeclEnd)
lexDeclEnd = symbol tokDeclEnd 

------------------------------------------
-- Operators
------------------------------------------

lexEQ' :: LexerF (Token tokEQ)
lexEQ' = symbol tokEQ 

lexEQ :: LexerF Op
lexEQ = Syntax.Common.EQ . locOf <$> symbol tokEQ 

lexNEQ :: LexerF Op
lexNEQ = NEQ . locOf <$> symbol tokNEQ 

lexNEQU :: LexerF Op
lexNEQU = NEQU . locOf <$> symbol tokNEQU 

lexGT :: LexerF Op
lexGT =  Syntax.Common.GT . locOf <$> symbol tokGT

lexGTE :: LexerF Op
lexGTE = GTE . locOf <$> symbol tokGTE 

lexGTEU :: LexerF Op
lexGTEU = GTEU . locOf <$> symbol tokGTEU 

lexLT :: LexerF Op
lexLT = Syntax.Common.LT . locOf <$> symbol tokLT 

lexLTE :: LexerF Op
lexLTE = LTE . locOf <$> symbol tokLTE 

lexLTEU :: LexerF Op
lexLTEU = LTEU . locOf <$> symbol tokLTEU 

lexImpl :: LexerF Op
lexImpl = Implies . locOf <$> symbol tokImpl

lexImplU :: LexerF Op
lexImplU = ImpliesU . locOf <$> symbol tokImplU

lexConj :: LexerF Op
lexConj = Conj . locOf <$> symbol tokConj

lexConjU :: LexerF Op
lexConjU = ConjU . locOf <$> symbol tokConjU

lexDisj :: LexerF Op
lexDisj = Disj . locOf <$> symbol tokDisj

lexDisjU :: LexerF Op
lexDisjU = DisjU . locOf <$> symbol tokDisjU

lexNeg :: LexerF Op
lexNeg = Neg . locOf <$> symbol tokNeg

lexNegU :: LexerF Op
lexNegU = NegU . locOf <$> symbol tokNegU

lexAdd :: LexerF Op
lexAdd = Add . locOf <$> symbol tokAdd

lexSub :: LexerF Op
lexSub = Sub . locOf <$> symbol tokSub

lexMul :: LexerF Op
lexMul = Mul . locOf <$> symbol tokMul

lexDiv :: LexerF Op
lexDiv = Div . locOf <$> symbol tokDiv

lexMod :: LexerF Op
lexMod = Mod . locOf <$> symbol tokMod

lexOps :: LexerF Op
lexOps = choice [
    lexEQ, lexNEQ, lexNEQU, 
    lexGT, lexGTE, lexGTEU,
    lexLT, lexLTE, lexLTEU,
    lexImpl, lexImplU,
    lexConj, lexConjU, lexDisj, lexDisjU,
    lexNeg, lexNEQU, lexAdd, lexSub, lexMul, lexDiv, lexMod
  ] <?> "operators"

------------------------------------------
-- literals
------------------------------------------

lexUpper :: LexerF (Text, Loc)
lexUpper =  lexeme . try . withPredicate notUpperKeywords $ do
  x <- upperChar
  xs <- many . satisfy $ (\c -> isAlphaNum c || c == '_' || c == '\'')
  return $ tokensToChunk (Proxy :: Proxy Text) (x : xs)

lexLower :: LexerF (Text, Loc)
lexLower = lexeme . try . withPredicate notLowerKeywords $ do
  x <- lowerChar 
  xs <- many . satisfy $ (\c -> isAlphaNum c || c == '_' || c == '\'')
  return $ tokensToChunk (Proxy :: Proxy Text) (x : xs)

lexTrue :: LexerF Lit
lexTrue = LitBool True . locOf <$> symbol tokTrue

lexFalse :: LexerF Lit
lexFalse = LitBool False . locOf <$> symbol tokFalse

lexInt :: LexerF Lit
lexInt = uncurry LitInt <$> lexeme Lex.decimal

lexLits :: LexerF Lit
lexLits = choice [lexTrue, lexFalse, lexInt] <?> "literals"

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
  start <- getCurLoc
  x <- m
  end <- getEndLoc
  return (x, start <--> end)

withLoc :: Located a => Lexer a -> Lexer (a, Loc)
withLoc p = do
  x <- p
  loc <- locOf <$> p
  return (x, loc)

-- NOTE : make sure no space consumed after parser m
notFollowedBySymbol :: LexerF a -> LexerF a
notFollowedBySymbol m = ParseFunc (\sc' -> (↓) m (return ()) <* notFollowedBy (satisfy isSymbol) <* sc')
  
  -- m <* notFollowedBy (satisfy isSymbol)

withPredicate :: (a -> Bool) -> Lexer a -> Lexer a
withPredicate f p = do
  o <- getOffset
  x <- p
  if f x
    then return x
    else do
      setOffset o
      fail "using keyword as variable name"
