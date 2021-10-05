{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Syntax.Parser.Token where

import           Data.Text                      ( Text )

------------------------------------------
-- tokens
------------------------------------------

tokLineComment :: Text
tokLineComment = "--"

tokBlockCommentStart :: Text
tokBlockCommentStart = "{{"

tokBlockCommentEnd :: Text
tokBlockCommentEnd = "}}"

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

tokData :: Text
tokData = "data"

tokArray :: Text
tokArray = "array"

tokOf :: Text
tokOf = "of"

tokNew :: Text
tokNew = "new"

tokDispose :: Text
tokDispose = "dispose"

tokRange :: Text
tokRange = ".."

tokGuardBar :: Text
tokGuardBar = "|"

tokArrow :: Text
tokArrow = "->"

tokArrowU :: Text
tokArrowU = "→"

tokCase :: Text
tokCase = "case"

tokStar :: Text
tokStar = "*"

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
tokBackSlash = "\\"

tokDeclStart :: Text
tokDeclStart = "{:"

tokDeclEnd :: Text
tokDeclEnd = ":}"

tokBlockStart :: Text
tokBlockStart = "|["

tokBlockEnd :: Text
tokBlockEnd = "]|"

------------------------------------------
-- Operators
------------------------------------------

tokEQProp :: Text
tokEQProp = "<=>"

tokEQPropU :: Text
tokEQPropU = "≡"

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
tokNegU = "¬"

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

tokMax :: Text
tokMax = "↑"

tokMin :: Text
tokMin = "↓"

tokPointsTo :: Text
tokPointsTo = "↦"

tokSConj :: Text
tokSConj = "٭"

tokSImp :: Text
tokSImp = "-٭"

tokExp :: Text
tokExp = "^"

tokSum :: Text
tokSum = "Σ"

tokPi :: Text
tokPi = "∏"

tokForall :: Text
tokForall = "∀"

tokExists :: Text
tokExists = "∃"

tokHash :: Text
tokHash = "#"

------------------------------------------
-- literals
------------------------------------------

tokTypeInt :: Text
tokTypeInt = "Int"

tokTypeBool :: Text
tokTypeBool = "Bool"

tokTypeChar :: Text
tokTypeChar = "Char"

tokTrue :: Text
tokTrue = "True"

tokFalse :: Text
tokFalse = "False"

tokUnderscore :: Text
tokUnderscore = "_"

notLowerKeywords :: Text -> Bool
notLowerKeywords t = t `notElem` lowerKeywords

notUpperKeywords :: Text -> Bool
notUpperKeywords t = t `notElem` upperKeywords

lowerKeywords :: [Text]
lowerKeywords =
  [ tokSkip
  , tokAbort
  , tokIf
  , tokFi
  , tokDo
  , tokOd
  , tokBnd
  , tokCon
  , tokVar
  , tokLet
  , tokArray
  , tokOf
  , tokNew
  , tokDispose
  ]

upperKeywords :: [Text]
upperKeywords = [tokTypeInt, tokTypeBool, tokTypeChar, tokTrue, tokFalse]
