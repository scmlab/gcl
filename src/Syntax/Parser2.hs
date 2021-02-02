{-# OPTIONS_GHC -fwarn-incomplete-patterns #-}
{-# LANGUAGE OverloadedStrings #-}
module Syntax.Parser2 where

import Syntax.Concrete (Token, SepBy(..), Interval(..), EndpointOpen(..), EndpointClose(..), GdCmd(..), Stmt(..), Program(..), Op(..),  Expr(..), TBase(..), Interval, Type(..), Name, Declaration(..))
import Syntax.Parser.Lexer2
import Syntax.Parser.Util2
import Syntax.Parser.Token
import Control.Monad (void)
import Control.Monad.Combinators.Expr (makeExprParser, Operator(..))
import Control.Applicative.Combinators ((<|>), some, sepBy1, between, many, choice)
import Data.Loc (Located(..), (<-->))
import Data.Text.Lazy (Text)
import Text.Megaparsec (MonadParsec(..), (<?>), parse)
import qualified Text.Megaparsec.Char.Lexer as Lex
import Text.Megaparsec.Char.Lexer (IndentOpt(..))

type Parser = Lexer
-- type SyntacticError = (Loc, String)

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
  Program <$> many pDeclaration <*> many pStmt

------------------------------------------
-- parse Declaration
------------------------------------------

pDeclaration :: Parser Declaration
pDeclaration = choice [pConstantDecl, pConstDeclWithProp, pVarDecl, pVarDeclWithProp, pLetDecl] <?> "declaration"

pConstantDecl :: Parser Declaration
pConstantDecl = 
  ConstDecl 
  <$> lexCon 
  <*> pList upperName
  <*> lexColon
  <*> pType

pConstDeclWithProp :: Parser Declaration
pConstDeclWithProp = 
  ConstDeclWithProp
  <$> lexCon 
  <*> pList upperName
  <*> lexColon 
  <*> pType
  <*> lexBraceStart 
  <*> pExpr
  <*> lexBraceEnd 

pVarDecl :: Parser Declaration 
pVarDecl = 
  VarDecl
  <$> lexVar 
  <*> pList lowerName
  <*> lexColon 
  <*> pType

pVarDeclWithProp :: Parser Declaration 
pVarDeclWithProp = 
  VarDeclWithProp
  <$> lexVar
  <*> pList lowerName
  <*> lexColon 
  <*> pType
  <*> lexBraceStart 
  <*> pExpr
  <*> lexBraceEnd 

pLetDecl :: Parser Declaration 
pLetDecl = 
  LetDecl
  <$> lexLet 
  <*> upperName
  <*> many lowerName
  <*> lexEQ'
  <*> pExpr
  
------------------------------------------
-- parse Stmt
------------------------------------------

pStmt :: Parser Stmt 
pStmt = choice [
          withLoc (Skip <$ lexSkip),
          withLoc (Abort <$ lexAbort),
          Assign <$> pList upperName <*> lexAssign <*> pList pExpr,
          Assert <$> lexBraceStart <*> pExpr <*> lexBraceEnd ,
          LoopInvariant 
            <$> lexBraceStart 
            <*> pExpr 
            <*> lexComma 
            <*> lexBnd 
            <*> lexColon 
            <*> pExpr 
            <*> lexBraceEnd,
          Do <$> lexDo <*> pIndentGdCmds <*> lexOd,
          If <$> lexIf <*> pIndentGdCmds <*> lexFi,
          withLoc (SpecQM <$ lexQM),
          Spec <$> lexSpecStart <*> lexSpecEnd,
          Proof <$> lexProofStart <*> lexProofEnd
        ] <?> "statements"

pIndentGdCmds :: Parser (SepBy tokGuardBar GdCmd)
pIndentGdCmds = Lex.indentBlock scn pIndentSepBy

pIndentSepBy :: Parser (IndentOpt Parser (SepBy sep GdCmd) GdCmd)
pIndentSepBy = return $ IndentSome Nothing f pGdCmd
  where
    f [] = undefined 
    f [x] = return (Head x)
    f (x : xs) = Delim x <$> lexGuardBar <*> f xs

pGdCmd :: Parser GdCmd 
pGdCmd = 
  Lex.indentBlock sc p
  where 
    p = return $ IndentSome Nothing f pStmt
    f stmts = GdCmd <$> pExpr <*> lexArrow <*> return stmts

------------------------------------------
-- parse Type
------------------------------------------

pType :: Parser Type
pType = choice [
    TParen <$> lexParenStart <*> pType <*> lexParenEnd,
    TBase <$> pBase,
    TArray <$> lexArray <*> pInterval <*> lexOf <*> pType,
    TFunc <$> pType <*> lexArrow <*> pType,
    TVar <$> lowerName 
  ]

pBase :: Parser TBase
pBase = withLoc . 
  choice $ [
    TInt <$ lexTypeInt,
    TBool <$ lexTypeBool,
    TChar <$ lexTypeChar  
  ]

pInterval :: Parser Interval
pInterval = 
  Interval <$> pEndpointOpen <*> lexRange <*> pEndpointClose

pEndpointOpen :: Parser EndpointOpen
pEndpointOpen = 
  IncludingOpening <$> lexBracketStart <*> pExpr 
  <|> ExcludingOpening <$> lexParenStart <*> pExpr

pEndpointClose :: Parser EndpointClose
pEndpointClose =
  IncludingClosing <$> pExpr <*> lexBracketEnd 
  <|> ExcludingClosing <$> pExpr <*> lexParenEnd 

------------------------------------------
-- parse Expr
------------------------------------------

pExpr :: Parser Expr
pExpr = makeExprParser pTerm opTable <?> "expression"

opTable :: [[Operator Parser Expr]] 
opTable = [
    [Postfix pApp],
    [InfixL $ pBinary lexMod],
    [InfixL (pBinary lexMul), InfixL (pBinary lexDiv), InfixL (pBinary lexAdd), InfixL (pBinary lexSub)],
    [InfixL (pBinary lexNEQ), InfixL (pBinary lexLT), InfixL (pBinary lexLTE), InfixL (pBinary lexGT), InfixL (pBinary lexGTE)],
    [InfixL (pBinary lexEQ)],
    [Prefix (pUnary lexNeg)],
    [InfixL (pBinary lexConj)],
    [InfixL (pBinary lexDisj)],
    [InfixL (pBinary lexImpl)]
  ]

pTerm :: Parser Expr 
pTerm = 
  choice [
      Lit <$> lexLits,
      Var <$> lowerName,
      Const <$> upperName,
      Op <$> lexOps,
      pQuant
    ] <?> "term"

pQuant :: Parser Expr
pQuant =
  Quant 
  <$> lexQuantStarts 
  <*> choice [Left <$> lexOps, Right <$> pExpr] 
  <*> some lowerName
  <*> lexColon 
  <*> pExpr
  <*> lexColon 
  <*> pExpr
  <*> lexQuantEnds 

-- pLam :: Parser Expr
-- pLam = do
--   void lexBackSlash 
--   vars <- some (getLoc lexLower)
--   expr <- pExpr
--   return (foldr f expr vars) <?> "lambda"
--   where
--     f (v, loc) e = Lam v e

pApp :: Parser (Expr -> Expr)
pApp = do
  terms <- many pTerm
  return $ \func -> do
    foldl App func terms
  
pBinary :: Parser Op -> Parser (Expr -> Expr -> Expr)
pBinary m = do
  op <- m
  return $ \x y -> App (App (Op op) x) y

pUnary :: Parser Op -> Parser (Expr -> Expr)
pUnary m = do
  op <- m
  return $ \x -> App (Op op) x

------------------------------------------
-- combinators
------------------------------------------
upperName :: Parser Name
upperName = textToName lexUpper

lowerName :: Parser Name
lowerName = textToName lexLower

parens :: Parser a -> Parser a
parens = between lexParenStart lexParenEnd

braces :: Parser a -> Parser a
braces = between lexBraceStart lexBraceEnd 

brackets :: Parser a -> Parser a
brackets = between lexBracketStart lexBracketEnd 

pSepBy :: Parser (Token sep) -> Parser a -> Parser (SepBy sep a)
pSepBy delim p = do
  x <- p
  let f = return (Head x)
  let g = Delim x <$> delim <*> pSepBy delim p
  try g <|> f

pList :: Parser a -> Parser (SepBy tokComma a)
pList = pSepBy lexComma