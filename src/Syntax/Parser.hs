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
import Control.Monad.Trans (lift)

type Parser = Lexer
type ParserF = LexerF

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
pDeclaration = Lex.lineFold scn (parser p)
  where
    p =
      choice [ try pConstDeclWithProp,
          pConstDecl,
          try pVarDeclWithProp,
          pVarDecl,
          pLetDecl
        ]
        <* lift scn
        <?> "declaration"

pConstDecl :: ParserF Declaration
pConstDecl =
  ConstDecl
    <$> lexCon
    <*> pList upperName
    <*> lexColon
    <*> pType'

pConstDeclWithProp :: ParserF Declaration
pConstDeclWithProp =
  ConstDeclWithProp
    <$> lexCon
    <*> pList upperName
    <*> lexColon
    <*> pType'
    <*> lexBraceStart
    <*> pExpr'
    <*> lexBraceEnd

pVarDecl :: ParserF Declaration
pVarDecl =
  VarDecl
    <$> lexVar
    <*> pList lowerName
    <*> lexColon
    <*> pType'

pVarDeclWithProp :: ParserF Declaration
pVarDeclWithProp =
  VarDeclWithProp
    <$> lexVar
    <*> pList lowerName
    <*> lexColon
    <*> pType'
    <*> lexBraceStart
    <*> pExpr'
    <*> lexBraceEnd

pLetDecl :: ParserF Declaration
pLetDecl =
  LetDecl
    <$> lexLet
    <*> upperName
    <*> many lowerName
    <*> lexEQ'
    <*> pExpr'

------------------------------------------
-- parse Stmt
------------------------------------------

pStmts :: Parser [Stmt]
pStmts = many (pStmt <* scn) <?> "statements"

-- NOTE :: this function doesn't consume newline after finish parsing the statement
pStmt :: Parser Stmt
pStmt = Lex.lineFold scn ((↓) pStmt') <?> "statement"

pStmt' :: ParserF Stmt
pStmt' =
  choice [
    pSkip,
    pAbort,
    try pAssert,
    try pLoopInvariant,
    try (lift pDo),
    try (lift pIf),
    try pAssign,
    pSpecQM,
    pSpec,
    pProof
  ] <* lift sc
  <?> "statement"

pSkip :: ParserF Stmt
pSkip = Skip . locOf <$> lexSkip

pAbort :: ParserF Stmt
pAbort = Abort . locOf <$> lexAbort

pAssign :: ParserF Stmt
pAssign = Assign <$> pList lowerName <*> lexAssign <*> pList pExpr'

pAssert :: ParserF Stmt
pAssert = Assert <$> lexBraceStart <*> pExpr' <*> lexBraceEnd

pLoopInvariant :: ParserF Stmt
pLoopInvariant =
  LoopInvariant
    <$> lexBraceStart
    <*> pExpr'
    <*> lexComma
    <*> lexBnd
    <*> lexColon
    <*> pExpr'
    <*> lexBraceEnd

pDo :: Parser Stmt
pDo = do
  (tDo, gdcmds, tOd) <- pIfoDoHelper lexDo lexOd
  return $ Do tDo gdcmds tOd

pIf :: Parser Stmt
pIf = do
    (tIf, gdcmds, tFi) <- pIfoDoHelper lexIf lexFi
    return $ If tIf gdcmds tFi

pGdCmd :: Parser GdCmd
pGdCmd = Lex.indentBlock scn p
  where
    p = do
      ref <- Lex.indentLevel
      gd <- pExpr
      arrow <- parser lexArrow . void $ Lex.indentGuard scn Ord.GT ref
      pos <- Lex.indentLevel
      s0 <- pStmt
      return $ Lex.IndentMany (Just pos) (\ss -> return $ GdCmd gd arrow (s0 : ss) ) pStmt

pSpecQM :: ParserF Stmt
pSpecQM = SpecQM . locOf <$> lexQM

pSpec :: ParserF Stmt
pSpec = Spec <$> lexSpecStart <*> lexSpecEnd

pProof :: ParserF Stmt
pProof = Proof <$> lexProofStart <*> lexProofEnd

------------------------------------------
-- parse Type
------------------------------------------

pType :: Parser Type
pType = (↓) pType' scn <?> "type"

pType' :: ParserF Type
pType' = makeExprParser pType'Term [[InfixR pFunction]] <* 
  (↑) (\sc' -> try sc' <|> sc) <?> "type"

pType'Term :: ParserF Type
pType'Term = choice [pParensType, pArray, pBase]

pFunction :: ParserF (Type -> Type -> Type)
pFunction = do
  arrow <- lexArrow
  return $ \t1 t2 -> TFunc t1 arrow t2

pParensType :: ParserF Type
pParensType = TParen <$> lexParenStart <*> pType' <*> lexParenEnd

pArray :: ParserF Type
pArray = TArray <$> lexArray <*> pInterval <*> lexOf <*> pType'

pBase :: ParserF Type
pBase =
  TBase <$> choice [
      TInt . locOf <$> lexTypeInt,
      TBool . locOf <$> lexTypeBool,
      TChar . locOf <$> lexTypeChar
    ]

pInterval :: ParserF Interval
pInterval = Interval <$> pEndpointOpen <*> lexRange <*> pEndpointClose

pEndpointOpen :: ParserF EndpointOpen
pEndpointOpen =
  (IncludingOpening <$> lexBracketStart <*> pExpr')
    <|> (ExcludingOpening <$> lexParenStart <*> pExpr')

pEndpointClose :: ParserF EndpointClose
pEndpointClose =
  try (IncludingClosing <$> pExpr' <*> lexBracketEnd)
    <|> (ExcludingClosing <$> pExpr' <*> lexParenEnd)

------------------------------------------
-- parse Expr
------------------------------------------

pExpr :: Parser Expr
pExpr = (↓) pExpr' scn <?> "expression"

pExpr' :: ParserF Expr
pExpr' = makeExprParser pExprArith chainOpTable <* (↑) (\sc' -> try sc' <|> sc) <?> "expression"

chainOpTable :: [[Operator ParserF Expr]]
chainOpTable = [
    [
      InfixL . pChain . choice $ [lexNEQ, lexNEQU],
      InfixL . pChain $ lexLT,
      InfixL . pChain . choice $ [lexLTE, lexLTEU],
      InfixL . pChain $ lexGT,
      InfixL . pChain . choice $ [lexGTE, lexGTEU]
    ],
    [InfixL . pChain $ lexEQ],
    [InfixL . pChain . choice $ [lexConj, lexConjU]],
    [InfixL . pChain . choice $ [lexDisj, lexDisjU]],
    [InfixL . pChain . choice $ [lexImpl, lexImplU]]
  ]

pExprArith :: ParserF Expr
pExprArith = makeExprParser pTerm arithTable <* (↑) (\sc' -> try sc' <|> sc)

arithTable :: [[Operator ParserF Expr]]
arithTable = [
    [Postfix pApp],
    [InfixL (pBinary lexMod)],
    [InfixL (pBinary lexMul), InfixL (pBinary lexDiv)],
    [InfixL (pBinary lexAdd), InfixL (pBinary lexSub)],
    [Prefix . pUnary . choice $ [lexNeg, lexNegU]]
  ]

pTerm :: ParserF Expr
pTerm = choice [pParen, pLit, pVar, pConst, pQuant] <?> "term"

pParen :: ParserF Expr
pParen = Paren <$> lexParenStart <*> pExpr' <*> lexParenEnd

pLit :: ParserF Expr
pLit = Lit <$> lexLits

pVar :: ParserF Expr
pVar = Var <$> lowerName

pConst :: ParserF Expr
pConst = Const <$> upperName

pQuant :: ParserF Expr
pQuant =
  Quant
    <$> lexQuantStarts
    <*> qOp
    <*> qNames
    <*> lexColon
    <*> pExpr'
    <*> lexColon
    <*> pExpr'
    <*> lexQuantEnds
  where
    qOp = choice [Left <$> lexOps, Right <$> pTerm]
    qNames = sepBy1 lowerName . try . (↑) $ id

pApp :: ParserF (Expr -> Expr)
pApp = do
  terms <- many pTerm
  return $ \func -> do
    foldl App func terms

pChain :: ParserF Op -> ParserF (Expr -> Expr -> Expr)
pChain m = do
  -- NOTE: operator cannot be followed by any symbol
  op <- try (notFollowedBySymbol m)
  return $ \x y -> Chain x op y

pBinary :: ParserF Op -> ParserF (Expr -> Expr -> Expr)
pBinary m = do
  -- NOTE: operator cannot be followed by any symbol
  op <- try (notFollowedBySymbol m)
  return $ \x y -> App (App (Op op) x) y

pUnary :: ParserF Op -> ParserF (Expr -> Expr)
pUnary m = do
  -- NOTE: operator cannot be followed by any symbol
  op <- try (notFollowedBySymbol m)
  return $ \x -> App (Op op) x

------------------------------------------
-- combinators
------------------------------------------
upperName :: ParserF Name
upperName = uncurry Name <$> lexUpper

lowerName :: ParserF Name
lowerName = uncurry Name <$> lexLower

pSepBy :: ParserF (Token sep)
  -> ParserF a
  -> ParserF (SepBy sep a)
pSepBy delim p = do
  x <- p
  let f = return (Head x)
  let g = Delim x <$> delim <*> pSepBy delim p
  try g <|> f

pList :: ParserF a -> ParserF (SepBy tokComma a)
pList = pSepBy lexComma

pIfoDoHelper ::
  ParserF (Token s)
  -> ParserF (Token e)
  -> Parser (Token s, SepBy sep GdCmd, Token e)
pIfoDoHelper start end = pIndentSepBy start end pGdCmd lexGuardBar

pIndentSepBy ::
  ParserF (Token s)
  -> ParserF (Token e)
  -> Parser a
  -> ParserF (Token sep)
  -> Parser (Token s, SepBy sep a, Token e)
pIndentSepBy start end p delim = do
  ref <- Lex.indentLevel
  ts <- parser start . void . Lex.indentGuard scn Ord.GT $ ref
  pos <- Lex.indentLevel
  gds <- f ref pos
  void $ try (Lex.indentGuard scn Ord.EQ ref) <|> Lex.indentGuard scn Ord.GT ref
  te <- parser end sc
  return (ts, gds, te)
  where
    f ref pos = do
        x <- p
        let g1 = return (Head x)
        let g2 = do
              void $ Lex.indentGuard scn Ord.GT ref
              tok <- parser delim (void $ Lex.indentGuard sc Ord.EQ pos)
              Delim x tok <$> f ref pos
        try g2 <|> g1

