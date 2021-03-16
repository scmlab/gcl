{-# LANGUAGE OverloadedStrings #-}

module Syntax.Parser.Token where

import Data.Text.Lazy (Text)

------------------------------------------
-- tokens 
------------------------------------------

tokSkip :: Text
tokSkip = "skip"

tokAbort :: Text
tokAbort = "abort"

tokDo :: Text
tokDo = "do"

tokOd :: Text
tokOd = "od"

tokIf :: Text
tokIf = "if"

tokFi :: Text
tokFi = "fi"

tokBnd :: Text
tokBnd = "bnd"

tokQM :: Text
tokQM = "?"

tokCon :: Text
tokCon = "con"

tokVar :: Text
tokVar = "var"

tokLet :: Text
tokLet = "let"

tokArray :: Text
tokArray = "array"

tokOf :: Text
tokOf = "of"

tokRange :: Text
tokRange = ".."

tokGuardBar :: Text
tokGuardBar = "|"

tokArrow :: Text
tokArrow = "->" 

tokArrowU :: Text
tokArrowU = "→"

------------------------------------------
-- delimiters
------------------------------------------

tokSpace :: Text
tokSpace = " "

tokComma :: Text
tokComma = ","

tokColon :: Text
tokColon = ":"

tokSemi :: Text
tokSemi = ";"

tokAssign :: Text
tokAssign = ":="

tokSpecStart :: Text
tokSpecStart = "[!"

tokSpecEnd :: Text
tokSpecEnd = "!]"

tokParenStart :: Text
tokParenStart = "("

tokParenEnd :: Text
tokParenEnd = ")"

tokBracketStart :: Text
tokBracketStart = "["

tokBracketEnd :: Text
tokBracketEnd = "]"

tokBraceStart :: Text
tokBraceStart = "{"

tokBraceEnd :: Text
tokBraceEnd = "}"

tokQuantStarts :: Text
tokQuantStarts = "<|" 

tokQuantEnds :: Text
tokQuantEnds = "|>" 

tokQuantStartU :: Text
tokQuantStartU = "⟨"

tokQuantEndU :: Text
tokQuantEndU = "⟩"

tokProofStart :: Text
tokProofStart = "{-"

tokProofEnd :: Text
tokProofEnd = "-}"

tokBackSlash :: Text
tokBackSlash = ""

------------------------------------------
-- Operators
------------------------------------------

tokEQ :: Text
tokEQ = "="

tokNEQ :: Text
tokNEQ = "/=" 

tokNEQU :: Text
tokNEQU = "≠"

tokGT :: Text
tokGT = ">"

tokGTE :: Text
tokGTE = ">=" 

tokGTEU :: Text
tokGTEU = "≥"

tokLT :: Text
tokLT = "<"

tokLTE :: Text
tokLTE = "<="

tokLTEU :: Text
tokLTEU = "≤"

tokImpl :: Text
tokImpl = "=>" 

tokImplU :: Text
tokImplU = "⇒"

tokConj :: Text
tokConj = "&&" 

tokConjU :: Text
tokConjU = "∧"

tokDisj :: Text
tokDisj = "||" 

tokDisjU :: Text
tokDisjU = "∨"

tokNeg :: Text
tokNeg = "~" 

tokNegU :: Text
tokNegU =  "¬"

tokAdd :: Text
tokAdd = "+"

tokSub :: Text
tokSub = "-"

tokMul :: Text
tokMul = "*"

tokDiv :: Text
tokDiv = "/"

tokMod :: Text
tokMod = "%"

------------------------------------------
-- literals
------------------------------------------

tokTypeInt :: Text
tokTypeInt = "Int"

tokTypeBool :: Text
tokTypeBool = "Bool"

tokTypeChar :: Text
tokTypeChar = "Char"


notLowerKeyword :: Text -> Bool
notLowerKeyword t = 
  t /= tokSkip && t /= tokAbort && t /= tokIf && t /= tokFi && 
    t /= tokDo && t /= tokOd && t /= tokBnd && t /= tokCon && t /= tokVar &&
    t /= tokLet && t /= tokArray && t /= tokOf

notUpperKeyworkd :: Text -> Bool
notUpperKeyworkd t = 
  t /= tokTypeInt && t /= tokTypeBool && t /= tokTypeChar
