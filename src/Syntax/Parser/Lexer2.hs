{-# LANGUAGE OverloadedStrings #-}
-- {-# LANGUAGE TupleSections     #-}

module Syntax.Parser.Lexer2 where

import Control.Applicative.Combinators ((<|>), choice, some, many)
import Data.Void (Void)
import Data.Text.Lazy (Text)
import qualified Data.Text.Lazy as Text
import Text.Megaparsec (satisfy, (<?>), Stream(..),  Parsec )
import Text.Megaparsec.Char (tab, alphaNumChar, lowerChar, char, upperChar, space1)
import qualified Text.Megaparsec.Char.Lexer as Lex
import Data.Proxy (Proxy(Proxy))
import Control.Monad (void)
import Syntax.Concrete (Lit(..),  Op(..))
import Data.Char (isSpace)

type Lexer = Parsec Void Text

-- consume single space character
lexSpace :: Lexer Char 
lexSpace = char ' '

lexTab :: Lexer Char 
lexTab = tab

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
symbol :: Text -> Lexer ()
symbol t = void (Lex.symbol sc t <?> Text.unpack t)

------------------------------------------
-- tokens 
------------------------------------------
tokSkip :: Lexer ()
tokSkip = symbol "skip"

tokAbort :: Lexer ()
tokAbort = symbol "abort"

tokDo :: Lexer ()
tokDo = symbol "do"

tokOd :: Lexer ()
tokOd = symbol "od"

tokIf :: Lexer ()
tokIf = symbol "if"

tokFi :: Lexer ()
tokFi = symbol "fi"

tokBnd :: Lexer ()
tokBnd = symbol "bnd"

tokQM :: Lexer ()
tokQM = symbol "?"

tokCon :: Lexer ()
tokCon = symbol "con"

tokVar :: Lexer ()
tokVar = symbol "var"

tokLet :: Lexer ()
tokLet = symbol "let"

tokArray :: Lexer ()
tokArray = symbol "array"

tokOf :: Lexer ()
tokOf = symbol "of"

tokRange :: Lexer ()
tokRange = symbol ".."

tokGuardBar :: Lexer ()
tokGuardBar = symbol "|"

tokArrow :: Lexer ()
tokArrow = symbol "->" <|> symbol "→"

------------------------------------------
-- delimiters
------------------------------------------

tokSpace :: Lexer ()
tokSpace = symbol " "

tokComma :: Lexer ()
tokComma = symbol ","

tokColon :: Lexer ()
tokColon = symbol ":"

tokSemi :: Lexer ()
tokSemi = symbol ";"

tokBar :: Lexer ()
tokBar = symbol "|"

tokAssign :: Lexer ()
tokAssign = symbol ":="

tokSpecStart :: Lexer ()
tokSpecStart = symbol "{!"

tokSpecEnd :: Lexer ()
tokSpecEnd = symbol "!}"

tokParenStart :: Lexer ()
tokParenStart = symbol "("

tokParenEnd :: Lexer ()
tokParenEnd = symbol ")"

tokBracketStart :: Lexer ()
tokBracketStart = symbol "["

tokBracketEnd :: Lexer ()
tokBracketEnd = symbol "]"

tokBraceStart :: Lexer ()
tokBraceStart = symbol "{"

tokBraceEnd :: Lexer ()
tokBraceEnd = symbol "}"

tokQuantStarts :: [Lexer ()]
tokQuantStarts = [symbol "<|", symbol "⟨"]

tokQuantEnds :: [Lexer ()]
tokQuantEnds = [symbol "|>", symbol "⟩"]

-- tokQuantStartU :: Lexer ()
-- tokQuantStartU = symbol "⟨"

-- tokQuantEndU :: Lexer ()
-- tokQuantEndU = symbol "⟩"

tokProofStart :: Lexer ()
tokProofStart = symbol "{-"

tokProofEnd :: Lexer ()
tokProofEnd = symbol "-}"

tokBackSlash :: Lexer ()
tokBackSlash = symbol "\\"

------------------------------------------
-- Operators
------------------------------------------

tokEQ :: Lexer Op
tokEQ = Syntax.Concrete.EQ <$ symbol "="

tokNEQ :: Lexer Op
tokNEQ = NEQ <$ (symbol "/=" <|> symbol "≠")

tokGT :: Lexer Op
tokGT = Syntax.Concrete.GT <$ symbol ">"

tokGTE :: Lexer Op
tokGTE = GTE <$ (symbol ">=" <|> symbol "≥")

tokLT :: Lexer Op
tokLT = Syntax.Concrete.LT <$ symbol "<"

tokLTE :: Lexer Op
tokLTE = LTE <$ (symbol "<=" <|> symbol "≤")

tokImpl :: Lexer Op
tokImpl = Implies <$ (symbol "=>" <|> symbol "⇒")

tokConj :: Lexer Op
tokConj = Conj <$ (symbol "&&" <|> symbol "∧")

tokDisj :: Lexer Op
tokDisj = Disj <$ (symbol "||" <|> symbol "∨")

tokNeg :: Lexer Op
tokNeg = Neg <$ (symbol "~" <|> symbol "¬")

tokAdd :: Lexer Op
tokAdd = Add <$ symbol "+"

tokSub :: Lexer Op
tokSub = Sub <$ symbol "-"

tokMul :: Lexer Op
tokMul = Mul <$ symbol "*"

tokDiv :: Lexer Op
tokDiv = Div <$ symbol "/"

tokMod :: Lexer Op
tokMod = Mod <$ symbol "%"

tokOps :: Lexer Op
tokOps = choice [
    tokEQ, tokNEQ, 
    tokGT, tokGTE, 
    tokLT, tokLTE, 
    tokImpl, 
    tokConj, tokDisj, 
    tokNeg, tokAdd, tokSub, tokMul, tokDiv, tokMod
  ] <?> "operators"

------------------------------------------
-- literals
------------------------------------------

tokUpper :: Lexer Text
tokUpper = lexeme $ do
  x <- upperChar
  xs <- many . choice $ [alphaNumChar , char '_', char '\'']
  return $ tokensToChunk (Proxy :: Proxy Text) (x : xs)

tokLower :: Lexer Text
tokLower = lexeme $ do
  x <- lowerChar 
  xs <- many . choice $ [alphaNumChar, char '_', char '\'']
  return $ tokensToChunk (Proxy :: Proxy Text) (x : xs)

tokTrue :: Lexer Lit
tokTrue = Bol True <$ symbol "True"

tokFalse :: Lexer Lit
tokFalse = Bol False <$ symbol "False"

tokInt :: Lexer Lit
tokInt = Num <$> lexeme Lex.decimal 

tokLits :: Lexer Lit
tokLits = choice [tokTrue, tokFalse, tokInt] <?> "literals"

-- Note : Not sure if `tokSignedInt`, `tokFloat` will be usable
tokSignedInt :: Lexer Lit
tokSignedInt = Num <$> Lex.signed scn (lexeme Lex.decimal)

tokFloat :: Lexer Double
tokFloat = lexeme Lex.float