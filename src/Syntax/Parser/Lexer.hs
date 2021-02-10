{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
-- {-# LANGUAGE TupleSections     #-}

module Syntax.Parser.Lexer where

import Control.Monad (void)
import Control.Applicative.Combinators ((<|>), choice, some, many)
import Data.Void (Void)
import Data.Proxy (Proxy(Proxy))
import Data.Char (isSymbol, isSpace)
import Data.Text.Lazy (Text)
import Data.Loc (Located(..), Loc(..), (<-->), Pos)
import Text.Megaparsec (setOffset, getOffset, MonadParsec(try, notFollowedBy), getSourcePos, Stream(tokensToChunk), satisfy, (<?>), Parsec )
import Text.Megaparsec.Char (string, alphaNumChar, lowerChar, char, upperChar, space1)
import qualified Text.Megaparsec.Char.Lexer as Lex
import Syntax.Concrete (Lit(..),  Op(..), Token (..))
import Syntax.Parser.Token
import Syntax.Parser.Util (getSourcePos',  sourceToLoc, (<$.>) )
import qualified Data.Text.Lazy as Text

type LexicalError = Pos
type Lexer = Parsec Void Text

spaceNotNewline :: Lexer Char
spaceNotNewline = satisfy (\t -> isSpace t && t /= '\n' && t /= '\r' && t /= '\f' && t /= '\v')

scn :: Lexer ()
scn = Lex.space space1 skipLineComment skipBlockComment

sc :: Lexer ()
sc = Lex.space (void $ some spaceNotNewline) skipLineComment skipBlockComment

-- Note: Should consume the newline character or not ?
skipLineComment :: Lexer ()
skipLineComment = Lex.skipLineComment "--"

-- NOTE: Not sure what the symbol of block comment will be
skipBlockComment :: Lexer ()
skipBlockComment = Lex.skipBlockComment "--/" "/--"

-- wrapper of tokens
lexeme :: Lexer () -> Lexer a -> Lexer (a, Loc)
lexeme sc' = Lex.lexeme (try sc' <|> sc) . getLoc

symbol :: Text -> Lexer () -> Lexer (Token a)
symbol t sc' = do
  (_, loc) <- Lex.lexeme (try sc' <|> sc) . getLoc . string $ t
  case loc of
    NoLoc -> error "NoLoc when parsing token"
    Loc l r -> return $ Token l r

------------------------------------------
-- lexical token 
------------------------------------------ 
lexSkip :: Lexer () -> Lexer (Token tokSkip)
lexSkip = symbol tokSkip 

lexAbort :: Lexer () -> Lexer (Token tokAbort)
lexAbort = symbol tokAbort 

lexDo :: Lexer () -> Lexer (Token tokDo)
lexDo = symbol tokDo 

lexOd :: Lexer () -> Lexer (Token tokOd)
lexOd = symbol tokOd 

lexIf :: Lexer () -> Lexer (Token tokIf)
lexIf = symbol tokIf 

lexFi :: Lexer () -> Lexer (Token tokFi)
lexFi = symbol tokFi 

lexBnd :: Lexer () -> Lexer (Token tokBnd)
lexBnd = symbol tokBnd 

lexQM :: Lexer () -> Lexer (Token tokQM)
lexQM = symbol tokQM 

lexCon :: Lexer () -> Lexer (Token tokCon)
lexCon = symbol tokCon 

lexVar :: Lexer () -> Lexer (Token tokVar)
lexVar = symbol tokVar 

lexLet :: Lexer () -> Lexer (Token tokLet)
lexLet = symbol tokLet 

lexArray :: Lexer () -> Lexer (Token tokArray)
lexArray = symbol tokArray 

lexOf :: Lexer () -> Lexer (Token tokOf)
lexOf = symbol tokOf 

lexRange :: Lexer () -> Lexer (Token tokRange)
lexRange = symbol tokRange 

lexGuardBar :: Lexer () -> Lexer (Token tokGuardBar)
lexGuardBar = symbol tokGuardBar 

lexArrow :: Lexer () -> Lexer (Either (Token tokArrow) (Token tokArrowU))
lexArrow sc' = choice [Left <$> symbol tokArrow sc', Right <$> symbol tokArrowU sc']

------------------------------------------
-- delimiters
------------------------------------------

lexSpace :: Lexer () -> Lexer (Token tokSpace)
lexSpace = symbol tokSpace 

lexComma :: Lexer () -> Lexer (Token tokComma)
lexComma = symbol tokComma 

lexColon :: Lexer () -> Lexer (Token tokColon)
lexColon = symbol tokColon 

lexSemi :: Lexer () -> Lexer (Token tokSemi)
lexSemi = symbol tokSemi 

lexAssign :: Lexer () -> Lexer (Token tokAssign)
lexAssign = symbol tokAssign 

lexSpecStart :: Lexer () -> Lexer (Token tokSpecStart)
lexSpecStart = symbol tokSpecStart 

lexSpecEnd :: Lexer () -> Lexer (Token tokSpecEnd)
lexSpecEnd = symbol tokSpecEnd 

lexParenStart :: Lexer () -> Lexer (Token tokParenStart)
lexParenStart = symbol tokParenStart 

lexParenEnd :: Lexer () -> Lexer (Token tokParenEnd)
lexParenEnd = symbol tokParenEnd 

lexBracketStart :: Lexer () -> Lexer (Token tokBracketStart)
lexBracketStart = symbol tokBracketStart 

lexBracketEnd :: Lexer () -> Lexer (Token tokBracketEnd)
lexBracketEnd = symbol tokBracketEnd 

lexBraceStart :: Lexer () -> Lexer (Token tokBraceStart)
lexBraceStart = symbol tokBraceStart 

lexBraceEnd :: Lexer () -> Lexer (Token tokBraceEnd)
lexBraceEnd = symbol tokBraceEnd 

lexQuantStarts :: Lexer () -> Lexer (Either (Token tokQuantStarts) (Token tokQuantStartU))
lexQuantStarts sc' = choice [Left <$> symbol tokQuantStarts sc', Right <$> symbol tokQuantStartU sc']

lexQuantEnds :: Lexer () -> Lexer (Either (Token tokQuantEnds) (Token tokQuantEndU))
lexQuantEnds sc' = choice [Left <$> symbol tokQuantEnds sc', Right <$> symbol tokQuantEndU sc'] 

lexProofStart :: Lexer () -> Lexer (Token tokProofStart)
lexProofStart = symbol tokProofStart 

lexProofEnd :: Lexer () -> Lexer (Token tokProofEnd)
lexProofEnd = symbol tokProofEnd 

lexBackSlash :: Lexer () -> Lexer (Token tokBackSlash)
lexBackSlash = symbol tokBackSlash 

------------------------------------------
-- Operators
------------------------------------------

lexEQ' :: Lexer () -> Lexer (Token tokEQ)
lexEQ' = symbol tokEQ 

lexEQ :: Lexer () -> Lexer Op
lexEQ sc' = Syntax.Concrete.EQ . locOf <$> symbol tokEQ sc'

lexNEQ :: Lexer () -> Lexer Op
lexNEQ sc' = NEQ . locOf <$> symbol tokNEQ sc'

lexNEQU :: Lexer () -> Lexer Op
lexNEQU sc' = NEQU . locOf <$> symbol tokNEQU sc'

lexGT :: Lexer () -> Lexer Op
lexGT sc' =  Syntax.Concrete.GT . locOf <$> symbol tokGT sc' 

lexGTE :: Lexer () -> Lexer Op
lexGTE sc' = GTE . locOf <$> symbol tokGTE sc'

lexGTEU :: Lexer () -> Lexer Op
lexGTEU sc' = GTEU . locOf <$> symbol tokGTEU sc'

lexLT :: Lexer () -> Lexer Op
lexLT sc' = Syntax.Concrete.LT . locOf <$> symbol tokLT sc'

lexLTE :: Lexer () -> Lexer Op
lexLTE sc' = LTE . locOf <$> symbol tokLTE sc'

lexLTEU :: Lexer () -> Lexer Op
lexLTEU sc' = LTEU . locOf <$> symbol tokLTEU sc'

lexImpl :: Lexer () -> Lexer Op
lexImpl sc' = Implies . locOf <$> symbol tokImpl sc'

lexImplU :: Lexer () -> Lexer Op
lexImplU sc' = ImpliesU . locOf <$> symbol tokImplU sc'

lexConj :: Lexer () -> Lexer Op
lexConj sc' = Conj . locOf <$> symbol tokConj sc'

lexConjU :: Lexer () -> Lexer Op
lexConjU sc' = ConjU . locOf <$> symbol tokConjU sc'

lexDisj :: Lexer () -> Lexer Op
lexDisj sc' = Disj . locOf <$> symbol tokDisj sc'

lexDisjU :: Lexer () -> Lexer Op
lexDisjU sc' = DisjU . locOf <$> symbol tokDisjU sc'

lexNeg :: Lexer () -> Lexer Op
lexNeg sc' = Neg . locOf <$> symbol tokNeg sc'

lexNegU :: Lexer () -> Lexer Op
lexNegU sc' = NegU . locOf <$> symbol tokNegU sc'

lexAdd :: Lexer () -> Lexer Op
lexAdd sc' = Add . locOf <$> symbol tokAdd sc'

lexSub :: Lexer () -> Lexer Op
lexSub sc' = Sub . locOf <$> symbol tokSub sc'

lexMul :: Lexer () -> Lexer Op
lexMul sc' = Mul . locOf <$> symbol tokMul sc'

lexDiv :: Lexer () -> Lexer Op
lexDiv sc' = Div . locOf <$> symbol tokDiv sc'

lexMod :: Lexer () -> Lexer Op
lexMod sc' = Mod . locOf <$> symbol tokMod sc'

lexOps :: Lexer () -> Lexer Op
lexOps sc' = choice (fmap (\f -> f sc') [
    lexEQ, lexNEQ, lexNEQU, 
    lexGT, lexGTE, lexGTEU,
    lexLT, lexLTE, lexLTEU,
    lexImpl, lexImplU,
    lexConj, lexConjU, lexDisj, lexDisjU,
    lexNeg, lexNEQU, lexAdd, lexSub, lexMul, lexDiv, lexMod
  ]) <?> "operators"

------------------------------------------
-- literals
------------------------------------------

lexUpper :: Lexer () -> Lexer (Text, Loc)
lexUpper sc' = withPredicate notLowerKeyword . lexeme sc' $ do
  x <- upperChar
  xs <- many . choice $ [alphaNumChar , char '_', char '\'']
  return $ tokensToChunk (Proxy :: Proxy Text) (x : xs)

lexLower :: Lexer () -> Lexer (Text, Loc)
lexLower sc' = withPredicate notLowerKeyword . lexeme sc' $ do
  x <- lowerChar 
  xs <- many . choice $ [alphaNumChar, char '_', char '\'']
  return $ tokensToChunk (Proxy :: Proxy Text) (x : xs)

lexTrue :: Lexer () -> Lexer Lit
lexTrue = (LitBool True . locOf) <$.> symbol "True"

lexFalse :: Lexer () -> Lexer Lit
lexFalse = (LitBool False . locOf) <$.> symbol "False"

lexInt :: Lexer () -> Lexer Lit
lexInt sc' = uncurry LitInt <$> lexeme sc' Lex.decimal

lexLits :: Lexer () -> Lexer Lit
lexLits sc' = f [lexTrue, lexFalse, lexInt] <?> "literals"
  where
    f = choice . map (\g -> g sc')

lexTypeInt :: Lexer () -> Lexer (Token tokTypeInt)
lexTypeInt = symbol tokTypeInt

lexTypeBool :: Lexer () -> Lexer (Token tokTypeBool)
lexTypeBool = symbol tokTypeBool

lexTypeChar :: Lexer () -> Lexer (Token tokTypeChar)
lexTypeChar = symbol tokTypeChar


------------------------------------------
-- helper combinators
------------------------------------------

getLoc :: Lexer a -> Lexer (a, Loc)
getLoc m = do
  start <- getSourcePos
  x <- m
  end <- getSourcePos' 
  return (x, sourceToLoc start <--> sourceToLoc end)

withLoc :: Located a => Lexer a -> Lexer (a, Loc)
withLoc p = do
  x <- p
  loc <- locOf <$> p
  return (x, loc)

notFollowedBySymbol :: Lexer a -> Lexer a
notFollowedBySymbol m = m <* notFollowedBy (satisfy isSymbol)

withPredicate :: (a -> Bool) -> Lexer (a, Loc) -> Lexer (a, Loc)
withPredicate f p = do
  o <- getOffset
  (r, loc) <- p
  if f r
    then return (r, loc)
    else do
      setOffset o
      fail "using keyword as variable name"
