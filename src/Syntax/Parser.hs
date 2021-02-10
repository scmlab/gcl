{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fwarn-incomplete-patterns #-}

module Syntax.Parser where

import Control.Applicative.Combinators (choice, many, sepBy1, (<|>))
import Control.Monad.Combinators.Expr (Operator (..), makeExprParser)
import Data.Loc (Located (locOf))
import Data.Text.Lazy (Text)
import qualified Data.Ord as Ord
import Syntax.Concrete (Declaration (..), EndpointClose (..), EndpointOpen (..), Expr (..), GdCmd (..), Interval (..), Name (..), Op (..), Program (..), SepBy (..), Stmt (..), TBase (..), Token, Type (..))
import Syntax.Parser.Lexer
import Syntax.Parser.Util
import Text.Megaparsec (MonadParsec (..), parse, (<?>))
import qualified Text.Megaparsec.Char.Lexer as Lex
import Control.Monad (void)

type Parser = Lexer

------------------------------------------
-- parse Program
------------------------------------------

runParse :: Parser a -> FilePath -> Text -> Either [SyntacticError] a
runParse p filepath s =
  case parse p filepath s of
    Left e -> Left (fromParseErrorBundle e)
    Right x -> Right x

pProgram :: Parser Program
pProgram = do
  scn
  Program <$> many pDeclaration <*> pStmts

------------------------------------------
-- parse Declaration
------------------------------------------

pDeclaration :: Parser Declaration
pDeclaration = Lex.lineFold scn p
  where
    p sc' =
      f
        sc'
        [ try . pConstDeclWithProp,
          pConstDecl,
          try . pVarDeclWithProp,
          pVarDecl,
          pLetDecl
        ]
        <* scn
        <?> "declaration"
    f sc' = choice . map (\g -> g sc')

pConstDecl :: Parser () -> Parser Declaration
pConstDecl =
  ConstDecl
    <$.> lexCon
    <**> pList upperName
    <**> lexColon
    <**> pType'

pConstDeclWithProp :: Parser () -> Parser Declaration
pConstDeclWithProp =
  ConstDeclWithProp
    <$.> lexCon
    <**> pList upperName
    <**> lexColon
    <**> pType'
    <**> lexBraceStart
    <**> pExpr'
    <**> lexBraceEnd

pVarDecl :: Parser () -> Parser Declaration
pVarDecl =
  VarDecl
    <$.> lexVar
    <**> pList lowerName
    <**> lexColon
    <**> pType'

pVarDeclWithProp :: Parser () -> Parser Declaration
pVarDeclWithProp =
  VarDeclWithProp
    <$.> lexVar
    <**> pList lowerName
    <**> lexColon
    <**> pType'
    <**> lexBraceStart
    <**> pExpr'
    <**> lexBraceEnd

pLetDecl :: Parser () -> Parser Declaration
pLetDecl =
  LetDecl
    <$.> lexLet
    <**> upperName
    <**> (many . lowerName)
    <**> lexEQ'
    <**> pExpr'

------------------------------------------
-- parse Stmt
------------------------------------------

pStmts :: Parser [Stmt]
pStmts = many (pStmt <* scn) <?> "statements"

-- NOTE :: this function doesn't consume newline after finish parsing the statement
pStmt :: Parser Stmt
pStmt = Lex.lineFold scn pStmt' <?> "statement"

pStmt' :: Parser () -> Parser Stmt
pStmt' sc' =
  f
    [ 
      pSkip,
      pAbort,
      try . pAssert,
      try . pLoopInvariant,
      try . pDo,
      try . pIf,
      try . pAssign,
      pSpecQM,
      pSpec,
      pProof
    ] <* sc
    <?> "statement"
  where
    f = choice . map (\g -> g sc')

pSkip :: Parser () -> Parser Stmt
pSkip = (Skip . locOf) <$.> lexSkip

pAbort :: Parser () -> Parser Stmt
pAbort = (Abort . locOf) <$.> lexAbort

pAssign :: Parser () -> Parser Stmt
pAssign = Assign <$.> pList lowerName <**> lexAssign <**> pList pExpr'

pAssert :: Parser () -> Parser Stmt
pAssert = Assert <$.> lexBraceStart <**> pExpr' <**> lexBraceEnd

pLoopInvariant :: Parser () -> Parser Stmt
pLoopInvariant =
  LoopInvariant
    <$.> lexBraceStart
    <**> pExpr'
    <**> lexComma
    <**> lexBnd
    <**> lexColon
    <**> pExpr'
    <**> lexBraceEnd

pDo :: Parser () -> Parser Stmt
pDo = const pDoLineFold

pDoLineFold :: Parser Stmt
pDoLineFold = do
  (tDo, gdcmds, tOd) <- pIfoDoHelper lexDo lexOd
  return $ Do tDo gdcmds tOd

pIf :: Parser () -> Parser Stmt
pIf = const pIfLineFold

pIfLineFold :: Parser Stmt
pIfLineFold = do
    (tIf, gdcmds, tFi) <- pIfoDoHelper lexIf lexFi
    return $ If tIf gdcmds tFi

pGdCmd :: Parser GdCmd
pGdCmd = pGdCmdOneLine

pGdCmdOneLine :: Parser GdCmd
pGdCmdOneLine = Lex.indentBlock scn p
  where
    p = do
      ref <- Lex.indentLevel 
      gd <- pExpr
      arrow <- lexArrow . void $ Lex.indentGuard scn Ord.GT ref
      pos <- Lex.indentLevel
      s0 <- pStmt
      return $ Lex.IndentMany (Just pos) (\ss -> return $ GdCmd gd arrow (s0 : ss) ) pStmt

pSpecQM :: Parser () -> Parser Stmt
pSpecQM = (SpecQM . locOf) <$.> lexQM

pSpec :: Parser () -> Parser Stmt
pSpec = Spec <$.> lexSpecStart <**> lexSpecEnd

pProof :: Parser () -> Parser Stmt
pProof = Proof <$.> lexProofStart <**> lexProofEnd

------------------------------------------
-- parse Type
------------------------------------------

pType :: Parser Type
pType = pType' scn <?> "type"

pType' :: Parser () -> Parser Type
pType' sc' = makeExprParser (pType'Term sc') [[InfixR (pFunction sc')]] <* (try sc' <|> sc) <?> "type"

pType'Term :: Parser () -> Parser Type
pType'Term sc' = choice . map (\f -> f sc') $ [pParensType, pArray, pBase]

pFunction :: Parser () -> Parser (Type -> Type -> Type)
pFunction sc' = do
  arrow <- lexArrow sc'
  return $ \t1 t2 -> TFunc t1 arrow t2

pParensType :: Parser () -> Parser Type
pParensType = TParen <$.> lexParenStart <**> pType' <**> lexParenEnd

pArray :: Parser () -> Parser Type
pArray = TArray <$.> lexArray <**> pInterval <**> lexOf <**> pType'

-- NOTE :: not sure if this work
pBase :: Parser () -> Parser Type
pBase sc' =
  TBase
    <$> f
      [ (TInt . locOf) <$.> lexTypeInt,
        (TBool . locOf) <$.> lexTypeBool,
        (TChar . locOf) <$.> lexTypeChar
      ]
  where
    f = choice . map (\g -> g sc')

pInterval :: Parser () -> Parser Interval
pInterval = Interval <$.> pEndpointOpen <**> lexRange <**> pEndpointClose

pEndpointOpen :: Parser () -> Parser EndpointOpen
pEndpointOpen sc' =
  (IncludingOpening <$.> lexBracketStart <**> pExpr' $ sc')
    <|> (ExcludingOpening <$.> lexParenStart <**> pExpr' $ sc')

pEndpointClose :: Parser () -> Parser EndpointClose
pEndpointClose sc' =
  try (IncludingClosing <$.> pExpr' <**> lexBracketEnd $ sc')
    <|> (ExcludingClosing <$.> pExpr' <**> lexParenEnd $ sc')

------------------------------------------
-- parse Expr
------------------------------------------

pExpr :: Parser Expr
pExpr = pExpr' scn <?> "expression"

pExpr' :: Parser () -> Parser Expr
pExpr' sc' = makeExprParser (pTerm sc') (opTable sc') <* (try sc' <|> sc) <?> "expression"

opTable :: Parser () -> [[Operator Parser Expr]]
opTable sc' =
  fmap
    (fmap (\f -> f sc'))
    [ [Postfix . pApp],
      [InfixL . pBinary . lexMod],
      [InfixL . pBinary . lexMul, InfixL . pBinary . lexDiv],
      [InfixL . pBinary . lexAdd, InfixL . pBinary . lexSub],
      [ InfixL . pBinary . lexNEQ,
        InfixL . pBinary . lexNEQU,
        InfixL . pBinary . lexLT,
        InfixL . pBinary . lexLTE,
        InfixL . pBinary . lexLTEU,
        InfixL . pBinary . lexGT,
        InfixL . pBinary . lexGTE,
        InfixL . pBinary . lexGTEU
      ],
      [InfixL . pBinary . lexEQ],
      [Prefix . pUnary . lexNeg, Prefix . pUnary . lexNegU],
      [InfixL . pBinary . lexConj, InfixL . pBinary . lexConjU],
      [InfixL . pBinary . lexDisj, InfixL . pBinary . lexDisjU],
      [InfixL . pBinary . lexImpl, InfixL . pBinary . lexImplU]
    ]

pTerm :: Parser () -> Parser Expr
pTerm sc' = f [pParen, pLit, pVar, pConst, pQuant] <?> "term"
  where
    f = choice . map (\g -> try . g $ sc')

pParen :: Parser () -> Parser Expr
pParen = Paren <$.> lexParenStart <**> pExpr' <**> lexParenEnd

pLit :: Parser () -> Parser Expr
pLit = Lit <$.> lexLits

pVar :: Parser () -> Parser Expr
pVar = Var <$.> lowerName

pConst :: Parser () -> Parser Expr
pConst = Const <$.> upperName

pQuant :: Parser () -> Parser Expr
pQuant =
  Quant
    <$.> lexQuantStarts
    <**> qOp
    <**> qNames
    <**> lexColon
    <**> pExpr'
    <**> lexColon
    <**> pExpr'
    <**> lexQuantEnds
  where
    qOp sc'' = choice . map (\g -> g sc'') $ [fmap Left . lexOps, fmap Right . pTerm]
    qNames sc'' = sepBy1 (lowerName sc'') (try sc'')

pApp :: Parser () -> Parser (Expr -> Expr)
pApp sc' = do
  terms <- many (pTerm sc')
  return $ \func -> do
    foldl App func terms

pBinary :: Parser Op -> Parser (Expr -> Expr -> Expr)
pBinary m = do
  -- NOTE: operator cannot be followed by any symbol
  op <- try (notFollowedBySymbol m)
  return $ \x y -> App (App (Op op) x) y

pUnary :: Parser Op -> Parser (Expr -> Expr)
pUnary m = do
  op <- try (notFollowedBySymbol m)
  return $ \x -> App (Op op) x

------------------------------------------
-- combinators
------------------------------------------
upperName :: Parser () -> Parser Name
upperName = uncurry Name <$.> lexUpper

lowerName :: Parser () -> Parser Name
lowerName = uncurry Name <$.> lexLower

pSepBy :: (Parser () -> Parser (Token sep)) 
  -> (Parser () -> Parser a) 
  -> Parser () -> Parser (SepBy sep a)
pSepBy delim p sc' = do
  x <- p sc'
  let f = return (Head x)
  let g = Delim x <$.> delim <**> pSepBy delim p $ sc'
  try g <|> f

pList :: (Parser () -> Parser a) -> Parser () -> Parser (SepBy tokComma a)
pList = pSepBy lexComma

pIfoDoHelper :: 
  (Parser () -> Parser (Token s))
  -> (Parser () -> Parser (Token e))
  -> Parser (Token s, SepBy sep GdCmd, Token e)
pIfoDoHelper start end = pIndentSepBy start end pGdCmd lexGuardBar 

pIndentSepBy ::
  (Parser () -> Parser (Token s))
  -> (Parser () -> Parser (Token e))
  -> Parser a 
  -> (Parser () -> Parser (Token sep)) 
  -> Parser (Token s, SepBy sep a, Token e)
pIndentSepBy start end p delim = do
  ref <- Lex.indentLevel 
  ts <- start . void . Lex.indentGuard scn Ord.GT $ ref
  pos <- Lex.indentLevel
  gds <- f ref pos
  void $ try (Lex.indentGuard scn Ord.EQ ref) <|> Lex.indentGuard scn Ord.GT ref
  te <- end sc
  return (ts, gds, te)
  where
    f ref pos = do
        x <- p
        let g1 = return (Head x)
        let g2 = do
              void $ Lex.indentGuard scn Ord.GT ref
              tok <- delim (void $ Lex.indentGuard sc Ord.EQ pos)
              Delim x tok <$> f ref pos
        try g2 <|> g1

