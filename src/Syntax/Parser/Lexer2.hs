{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
-- {-# LANGUAGE TupleSections     #-}

module Syntax.Parser.Lexer2 where

import Control.Monad (void)
import Control.Applicative.Combinators (sepBy1, (<|>), choice, some, many)
import Data.Void (Void)
import Data.Proxy (Proxy(Proxy))
import Data.Char (isSpace)
import Data.Text.Lazy (Text)
import qualified Data.Text.Lazy as Text
import Data.Loc (Loc(..), (<-->))
import Text.Megaparsec (Stream(tokensToChunk), MonadParsec(..), State(..), PosState(..), satisfy, (<?>), Parsec )
import Text.Megaparsec.Char (alphaNumChar, lowerChar, char, upperChar, space1)
import qualified Text.Megaparsec.Char.Lexer as Lex
import Syntax.Concrete (Name(..), Lit(..),  Op(..), Token (..))
import Syntax.Parser.Token
import Syntax.Parser.Util2 ( sourceToLoc )


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
lexeme :: Lexer a -> Lexer a
lexeme = Lex.lexeme sc

-- wrapper of tokens
symbol' :: Text -> Lexer Text
symbol' t = Lex.symbol sc t <?> Text.unpack t

symbol :: Text -> Lexer (Token a)
symbol t = do
  (_, loc) <- getLoc (Lex.symbol sc t <?> Text.unpack t)
  case loc of
    NoLoc -> error "NoLoc when parsing token"
    Loc l r -> return $ Token l r


------------------------------------------
-- lexical token 
------------------------------------------ 
lexSkip :: Lexer (Token tokSkip)
lexSkip = symbol tokSkip 

lexAbort :: Lexer (Token tokAbort)
lexAbort = symbol tokAbort 

lexDo :: Lexer (Token tokDo)
lexDo = symbol tokDo 

lexOd :: Lexer (Token tokOd)
lexOd = symbol tokOd 

lexIf :: Lexer (Token tokIf)
lexIf = symbol tokIf 

lexFi :: Lexer (Token tokFi)
lexFi = symbol tokFi 

lexBnd :: Lexer (Token tokBnd)
lexBnd = symbol tokBnd 

lexQM :: Lexer (Token tokQM)
lexQM = symbol tokQM 

lexCon :: Lexer (Token tokCon)
lexCon = symbol tokCon 

lexVar :: Lexer (Token tokVar)
lexVar = symbol tokVar 

lexLet :: Lexer (Token tokLet)
lexLet = symbol tokLet 

lexArray :: Lexer (Token tokArray)
lexArray = symbol tokArray 

lexOf :: Lexer (Token tokOf)
lexOf = symbol tokOf 

lexRange :: Lexer (Token tokRange)
lexRange = symbol tokRange 

lexGuardBar :: Lexer (Token tokGuardBar)
lexGuardBar = symbol tokGuardBar 

lexArrow :: Lexer (Either (Token tokArrow) (Token tokArrowU))
lexArrow = choice [Left <$> symbol tokArrow, Right <$> symbol tokArrowU]

------------------------------------------
-- delimiters
------------------------------------------

lexSpace :: Lexer (Token tokSpace)
lexSpace = symbol tokSpace 

lexComma :: Lexer (Token tokComma)
lexComma = symbol tokComma 

lexColon :: Lexer (Token tokColon)
lexColon = symbol tokColon 

lexSemi :: Lexer (Token tokSemi)
lexSemi = symbol tokSemi 

lexAssign :: Lexer (Token tokAssign)
lexAssign = symbol tokAssign 

lexSpecStart :: Lexer (Token tokSpecStart)
lexSpecStart = symbol tokSpecStart 

lexSpecEnd :: Lexer (Token tokSpecEnd)
lexSpecEnd = symbol tokSpecEnd 

lexParenStart :: Lexer (Token tokParenStart)
lexParenStart = symbol tokParenStart 

lexParenEnd :: Lexer (Token tokParenEnd)
lexParenEnd = symbol tokParenEnd 

lexBracketStart :: Lexer (Token tokBracketStart)
lexBracketStart = symbol tokBracketStart 

lexBracketEnd :: Lexer (Token tokBracketEnd)
lexBracketEnd = symbol tokBracketEnd 

lexBraceStart :: Lexer (Token tokBraceStart)
lexBraceStart = symbol tokBraceStart 

lexBraceEnd :: Lexer (Token tokBraceEnd)
lexBraceEnd = symbol tokBraceEnd 

lexQuantStarts :: Lexer (Either (Token tokQuantStarts) (Token tokQuantStartU))
lexQuantStarts = choice [Left <$> symbol tokQuantStarts, Right <$> symbol tokQuantStartU]

lexQuantEnds :: Lexer (Either (Token tokQuantEnds) (Token tokQuantEndU))
lexQuantEnds = choice [Left <$> symbol tokQuantEnds, Right <$> symbol tokQuantEndU] 

lexProofStart :: Lexer (Token tokProofStart)
lexProofStart = symbol tokProofStart 

lexProofEnd :: Lexer (Token tokProofEnd)
lexProofEnd = symbol tokProofEnd 

lexBackSlash :: Lexer (Token tokBackSlash)
lexBackSlash = symbol tokBackSlash 

------------------------------------------
-- Operators
------------------------------------------

lexEQ' :: Lexer (Token tokEQ)
lexEQ' = symbol tokEQ 

lexEQ :: Lexer Op
lexEQ = withLoc $ Syntax.Concrete.GT <$ symbol tokEQ 

lexNEQ :: Lexer Op
lexNEQ = withLoc $ NEQ <$ (symbol tokNEQ  <|> symbol tokNEQU)

lexGT :: Lexer Op
lexGT = withLoc $ Syntax.Concrete.GT <$ symbol tokGT 

lexGTE :: Lexer Op
lexGTE = withLoc $ GTE <$ (symbol tokGTE <|> symbol tokGTEU)

lexLT :: Lexer Op
lexLT = withLoc $ Syntax.Concrete.LT <$ symbol tokLT 

lexLTE :: Lexer Op
lexLTE = withLoc $ LTE <$ (symbol tokLTE <|> symbol tokLTEU)

lexImpl :: Lexer Op
lexImpl = withLoc $ Implies <$ (symbol tokImpl <|> symbol tokImplU)

lexConj :: Lexer Op
lexConj = withLoc $ Conj <$ (symbol tokConj <|> symbol tokConjU)

lexDisj :: Lexer Op
lexDisj = withLoc $ Disj <$ (symbol tokDisj <|> symbol tokDisjU)

lexNeg :: Lexer Op
lexNeg = withLoc $ Neg <$ (symbol tokNeg <|> symbol tokNegU)

lexAdd :: Lexer Op
lexAdd = withLoc $ Add <$ symbol tokAdd 

lexSub :: Lexer Op
lexSub = withLoc $ Sub <$ symbol tokSub 

lexMul :: Lexer Op
lexMul = withLoc $ Mul <$ symbol tokMul 

lexDiv :: Lexer Op
lexDiv = withLoc $ Div <$ symbol tokDiv 

lexMod :: Lexer Op
lexMod = withLoc $ Mod <$ symbol tokMod 

lexOps :: Lexer Op
lexOps = choice [
    lexEQ, lexNEQ, 
    lexGT, lexGTE, 
    lexLT, lexLTE, 
    lexImpl, 
    lexConj, lexDisj, 
    lexNeg, lexAdd, lexSub, lexMul, lexDiv, lexMod
  ] <?> "operators"

------------------------------------------
-- literals
------------------------------------------

lexUpper :: Lexer Text
lexUpper = lexeme $ do
  x <- upperChar
  xs <- many . choice $ [alphaNumChar , char '_', char '\'']
  return $ tokensToChunk (Proxy :: Proxy Text) (x : xs)

lexLower :: Lexer Text
lexLower = lexeme $ do
  x <- lowerChar 
  xs <- many . choice $ [alphaNumChar, char '_', char '\'']
  return $ tokensToChunk (Proxy :: Proxy Text) (x : xs)

lexTrue :: Lexer Lit
lexTrue = withLoc $ LitBool True <$ symbol "True"

lexFalse :: Lexer Lit
lexFalse = withLoc $ LitBool False <$ symbol "False"

lexInt :: Lexer Lit
lexInt = withLoc $ LitInt <$> lexeme Lex.decimal 

lexLits :: Lexer Lit
lexLits = choice [lexTrue, lexFalse, lexInt] <?> "literals"

-- Note : Not sure if `lexSignedInt`, `lexFloat` will be usable
lexSignedInt :: Lexer Lit
lexSignedInt = withLoc $ LitInt <$> Lex.signed scn (lexeme Lex.decimal)

lexFloat :: Lexer Double
lexFloat = lexeme Lex.float

lexTypeInt :: Lexer (Token tokTypeInt)
lexTypeInt = symbol tokTypeInt

lexTypeBool :: Lexer (Token tokTypeBool)
lexTypeBool = symbol tokTypeBool

lexTypeChar :: Lexer (Token tokTypeChar)
lexTypeChar = symbol tokTypeChar


------------------------------------------
-- helper combinators
------------------------------------------

-- parse one or more elements into a list
pToList :: Lexer a -> Lexer [a]
pToList meth = sepBy1 meth lexComma <?> "a list of elements seperated by commas"

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
