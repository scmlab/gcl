-- {-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE OverloadedStrings #-}
module Syntax.Parser2 where

import Syntax.Concrete (Interval(..), Endpoint(..), GdCmd(..), Defns, Stmt(..), Program(..), Op(..),  Expr(..), TBase(..), Interval, Type(..), Name, Declaration(..))
import Syntax.Parser.Lexer2
import Syntax.Parser.Util2
import Control.Monad (void)
import Control.Monad.Combinators.Expr (makeExprParser, Operator(..))
import Control.Applicative (Applicative(liftA2))
import Control.Applicative.Combinators ((<|>), some, sepBy1, between, many, optional, choice)
import Data.Loc (Loc, Located(..), (<-->))
import Data.Text.Lazy (Text)
import Text.Megaparsec (MonadParsec(tokens), (<?>), parse)
import qualified Text.Megaparsec.Char.Lexer as Lex
import Text.Megaparsec.Char.Lexer (IndentOpt(..))

type Parser = Lexer
type SyntacticError = (Loc, String)

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
  decls <- many pDeclaration
  exprs <- many pExpr
  defns <- pDefns
  stmts <- many pStmt
  withLoc . return $ Program decls exprs defns stmts

pDefns :: Parser Defns
pDefns = _

------------------------------------------
-- parse Declaration
------------------------------------------

pDeclaration :: Parser Declaration
pDeclaration = choice [pConstantDecl, pVariableDecl, pLetDecl] <?> "declaration"

pConstantDecl :: Parser Declaration
pConstantDecl = withLoc $ do
  tokCon 
  vars <- pToList upperName
  tokColon 
  t <- pType
  assertion <- optional pExpr
  return (ConstDecl vars t assertion) <?> "constant declaration"

pVariableDecl :: Parser Declaration 
pVariableDecl = withLoc $ do
  tokVar 
  vars <- pToList lowerName
  tokColon
  t <- pType
  assertion <- optional pExpr
  return (VarDecl vars  t assertion) <?> "variable declaration"

pLetDecl :: Parser Declaration 
pLetDecl = withLoc $ do
  tokLet
  n <- upperName
  args <- many tokLower
  void tokEQ
  (LetDecl n args <$> pExpr) <?> "let declaration"

------------------------------------------
-- parse Stmt
------------------------------------------

pStmt :: Parser Stmt 
pStmt = withLoc (choice [
          Skip <$ tokSkip,
          Abort <$ tokAbort,
          Assign <$> pToList upperName <* tokAssign <*> pToList pExpr,
          Assert <$> braces pExpr,
          braces (LoopInvariant <$> pExpr <* tokComma <* tokBnd <* tokColon <*> pExpr),
          Do <$> pDo,
          If <$> pIf,
          -- Do <$> sepBy1 pGdCmd tokGuardBar,
          -- If <$> sepBy1 pGdCmd tokGuardBar,
          SpecQM <$ tokQM,
          Spec <$ (tokSpecStart *> many pStmt <* many (tokens (/=) "!}") <* tokSpecEnd),
          Proof <$ (tokProofStart *> many pStmt <* many (tokens (/=) "-}")  <* tokProofEnd)
        ] <?> "statements")

pDo :: Parser [GdCmd]
pDo = Lex.indentBlock scn (tokDo *> pIndentGdCmds <* tokOd)

pIf :: Parser [GdCmd]
pIf = Lex.indentBlock scn (tokIf *> pIndentGdCmds <* tokFi)

pIndentGdCmds :: Parser (IndentOpt Parser [GdCmd] GdCmd)
pIndentGdCmds = do
  gd1 <- pGdCmd
  return $ IndentMany Nothing (return . (gd1 :)) (tokBar *> tokSpace *> pGdCmd)

pGdCmd :: Parser GdCmd 
pGdCmd = 
  Lex.indentBlock sc p
  where 
    p = do
      expr <- pExpr
      tokArrow 
      -- void newline
      return $ IndentSome Nothing (withLoc . return . GdCmd expr) pStmt

------------------------------------------
-- parse Type
------------------------------------------

pType :: Parser Type
pType = pFunction

pFunction :: Parser Type
pFunction = 
  foldr1 (\x y -> TFunc x y (x <--> y)) <$> sepBy1 pClosed tokArrow

pClosed :: Parser Type 
pClosed = choice [pBase, pArray, parens pFunction]

pBase :: Parser Type
pBase = withLoc $ choice [
      TBase TInt <$ symbol "Int",
      TBase TBool <$ symbol "Bool",
      TBase TChar <$ symbol "Char",
      TVar <$> lowerName
    ]

pArray :: Parser Type
pArray = do
  tokArray 
  i <- pInterval
  tokOf
  t <- pFunction
  return $ TArray i t (locOf t)

pInterval :: Parser Interval
pInterval = withLoc . liftA2 (<|>) parens brackets $ do
  e1 <- pEndpoint
  tokRange 
  Interval e1 <$> pEndpoint 

pEndpoint :: Parser Endpoint 
pEndpoint = 
  Excluding <$> pExpr <|> Including <$> pExpr

------------------------------------------
-- parse Expr
------------------------------------------

pExpr :: Parser Expr
pExpr = makeExprParser pTerm opTable <?> "expression"

opTable :: [[Operator Parser Expr]] 
opTable = [
    [Postfix pApp],
    [InfixL $ pBinary tokMod],
    [InfixL (pBinary tokMul), InfixL (pBinary tokDiv), InfixL (pBinary tokAdd), InfixL (pBinary tokSub)],
    [InfixL (pBinary tokNEQ), InfixL (pBinary tokLT), InfixL (pBinary tokLTE), InfixL (pBinary tokGT), InfixL (pBinary tokGTE)],
    [InfixL (pBinary tokEQ)],
    [Prefix (pUnary tokNeg)],
    [InfixL (pBinary tokConj)],
    [InfixL (pBinary tokDisj)],
    [InfixL (pBinary tokImpl)]
  ]

pTerm :: Parser Expr 
pTerm = 
  withLoc (choice [
      Lit <$> tokLits,
      Var <$> lowerName,
      Const <$> upperName,
      Op <$> tokOps,
      pQuant,
      Hole <$ tokQM
    ]) <?> "term"

pQuant :: Parser (Loc -> Expr)
pQuant =
  choice $ zipWith (curry f) tokQuantStarts tokQuantEnds
  where
    f (ts, te) = Quant
                  <$ ts
                  <*> pQuantOp
                  <*> some lowerName
                  <* tokColon 
                  <*> pExpr
                  <* tokColon
                  <*> pExpr
                  <* te
                        
pQuantOp :: Parser Expr 
pQuantOp = do
  op <- pTerm
  return $ case op of
    Op Add loc -> Op Sum loc
    Op Conj loc -> Op Conj loc
    Op Disj loc -> Op Disj loc
    others -> others

pLam :: Parser Expr
pLam = do
  tokBackSlash 
  vars <- some (getLoc tokLower)
  expr <- pExpr
  return (foldr f expr vars) <?> "lambda"
  where
    f (v, loc) e = Lam v e (loc <--> e)

pApp :: Parser (Expr -> Expr)
pApp = do
  terms <- many pTerm
  return $ \func -> do
    foldl (\f t -> App f t (f <--> t)) func terms
  
pBinary :: Parser Op -> Parser (Expr -> Expr -> Expr)
pBinary m = do
  (op, loc) <- getLoc m
  return $ \x y -> App (App (Op op loc) x (x <--> loc)) y (x <--> y)

pUnary :: Parser Op -> Parser (Expr -> Expr)
pUnary m = do
  (op, loc) <- getLoc m
  return $ \x -> App (Op op loc) x (x <--> loc)

------------------------------------------
-- combinators
------------------------------------------
upperName :: Parser Name
upperName = textToName tokUpper

lowerName :: Parser Name
lowerName = textToName tokLower

parens :: Parser a -> Parser a
parens = between tokParenStart tokParenEnd

braces :: Parser a -> Parser a
braces = between tokBraceStart tokBraceEnd 

brackets :: Parser a -> Parser a
brackets = between tokBracketStart tokBracketEnd 
