{-# LANGUAGE DataKinds #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PartialTypeSignatures #-}

module Syntax.Parser where

import Control.Applicative.Combinators (choice, eitherP, many, manyTill_, optional, sepBy1, (<|>))
import Control.Monad (void)
import Control.Monad.Combinators.Expr (Operator (..), makeExprParser)
import Control.Monad.Trans (lift)
import Data.Data (Proxy (Proxy))
import Data.Loc (Located (locOf))
import Data.Maybe (isJust)
import qualified Data.Ord as Ord
import Data.Text (Text)
import Syntax.Common (Name (..))
import Syntax.Concrete (BlockDeclaration (..), Decl (..), DeclProp (..), Declaration (..), EndpointClose (..), EndpointOpen (..), Expr (..), GdCmd (..), Interval (..), Op (..), Program (..), SepBy (..), Stmt (..), TBase (..), Token (..), Type (..))
import Syntax.Parser.Lexer
import Syntax.Parser.Util
import Text.Megaparsec (MonadParsec (..), Pos, anySingle, mkPos, parse, tokensToChunk, unPos, (<?>))
import Text.Megaparsec.Char (eol)
import qualified Text.Megaparsec.Char.Lexer as Lex

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
  Program <$> many (eitherP pDeclaration pBlockDeclaration) <*> pStmts <* eof

------------------------------------------
-- parse Declaration
------------------------------------------

pDeclaration :: Parser Declaration
pDeclaration = Lex.lineFold scn (parser p)
  where
    p =
      choice
        [ try pConstDeclWithProp,
          pConstDecl,
          try pVarDeclWithProp,
          pVarDecl,
          pLetDecl
        ]
        <* lift scn
        <?> "declaration"

pBlockDeclaration :: Parser BlockDeclaration
pBlockDeclaration =
  Lex.indentBlock scn p
  where
    d = Lex.lineFold scn (\sc' -> (,) <$> (↓) (pDecl upperName) sc' <*> (↓) (optional (eitherP pDeclProp pExpr')) sc')
    p = do
      bs <- (↓) lexDeclStart sc
      return (Lex.IndentMany Nothing (\ds -> BlockDecl bs ds <$> (↓) lexDeclEnd scn) d)

pDecl :: ParserF Name -> ParserF Decl
pDecl name =
  Decl
    <$> ( do
            ns <- pList name
            col <- lexColon
            t <- pType'
            return (ns, col, t)
        )

pDeclProp :: ParserF DeclProp
pDeclProp =
  DeclProp
    <$> ( do
            l <- lexBraceStart
            p <- pExpr'
            r <- lexBraceEnd
            return (l, p, r)
        )

pConstDecl :: ParserF Declaration
pConstDecl =
  ConstDecl
    <$> lexCon
    <*> pDecl upperName

pConstDeclWithProp :: ParserF Declaration
pConstDeclWithProp =
  ConstDeclWithProp
    <$> lexCon
    <*> pDecl upperName
    <*> pDeclProp

pVarDecl :: ParserF Declaration
pVarDecl =
  VarDecl
    <$> lexVar
    <*> pDecl lowerName

pVarDeclWithProp :: ParserF Declaration
pVarDeclWithProp =
  VarDeclWithProp
    <$> lexVar
    <*> pDecl lowerName
    <*> pDeclProp

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
  choice
    [ pSkip,
      pAbort,
      try pAssert,
      pLoopInvariant,
      pAssign,
      lift pDo,
      lift pIf,
      pSpecQM,
      lift pSpec,
      lift pProof
    ]
    <* lift sc
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

-- pGdCmd :: ParserF GdCmd
-- pGdCmd = (↑) (Lex.indentBlock scn . p)
--   where
--     p :: Parser () -> Parser (Lex.IndentOpt Parser GdCmd Stmt)
--     p sc' = do
--       gd <- pExpr
--       arrow <- (↓) lexArrow sc
--       sc'
--       pos <- Lex.indentLevel
--       s0 <- pStmt
--       return $ Lex.IndentMany (Just pos) (\ss -> return $ GdCmd gd arrow (s0 : ss) ) pStmt

pGdCmd :: ParserF GdCmd
pGdCmd = do
  gd <- pExpr'
  arrow <- lexArrow
  pos <- Lex.indentLevel
  stmt0 <- lift pStmt
  -- check if is end of line, e.g. have statements more than one
  isEol <- optional . try . lift $ eol
  done <- isJust <$> optional eof
  case (isEol, done) of
    (Just _, False) -> do
      stmts <- lift $ indentedItems (posMoveLeft pos 1) pos scn pStmt
      return (GdCmd gd arrow (stmt0 : stmts))
    _ -> return (GdCmd gd arrow [stmt0])
  where
    posMoveLeft pos i = mkPos (unPos pos - i)

pSpecQM :: ParserF Stmt
pSpecQM = SpecQM . locOf <$> lexQM

pSpec :: Parser Stmt
pSpec = do
  (ts, t, te) <- pBlock lexSpecStart lexSpecEnd anySingle
  return $ Spec ts (tokensToChunk (Proxy :: Proxy Text) t) te

pProof :: Parser Stmt
pProof = do
  (ts, _, te) <- pBlock lexProofStart lexProofEnd anySingle
  return $ Proof ts te

------------------------------------------
-- parse Type
------------------------------------------

pType :: Parser Type
pType = (↓) pType' scn <?> "type"

pType' :: ParserF Type
pType' =
  makeExprParser pType'Term [[InfixR pFunction]]
    <* (↑) (\sc' -> try sc' <|> sc) <?> "type"

pType'Term :: ParserF Type
pType'Term = choice [pParensType, pArrayType, pBase]

pFunction :: ParserF (Type -> Type -> Type)
pFunction = do
  arrow <- lexArrow
  return $ \t1 t2 -> TFunc t1 arrow t2

pParensType :: ParserF Type
pParensType = TParen <$> lexParenStart <*> pType' <*> lexParenEnd

pArrayType :: ParserF Type
pArrayType = TArray <$> lexArray <*> pInterval <*> lexOf <*> pType'

pBase :: ParserF Type
pBase =
  TBase
    <$> choice
      [ TInt . locOf <$> lexTypeInt,
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
chainOpTable =
  [ [ InfixL . pChain . choice $ [lexNEQ, lexNEQU],
      InfixL . pChain $ lexLT,
      InfixL . pChain . choice $ [lexLTE, lexLTEU],
      InfixL . pChain $ lexGT,
      InfixL . pChain . choice $ [lexGTE, lexGTEU]
    ],
    [InfixL . pChain $ lexEQ],
    [InfixL . pBinary . choice $ [lexConj, lexConjU]],
    [InfixL . pBinary . choice $ [lexDisj, lexDisjU]],
    [InfixL . pBinary . choice $ [lexImpl, lexImplU]]
  ]

pExprArith :: ParserF Expr
pExprArith = makeExprParser pTerm arithTable <* (↑) (\sc' -> try sc' <|> sc)

arithTable :: [[Operator ParserF Expr]]
arithTable =
  [ [Postfix pApp],
    [InfixL (pBinary lexMod)],
    [InfixL (pBinary lexMul), InfixL (pBinary lexDiv)],
    [InfixL (pBinary lexAdd), InfixL (pBinary lexSub)],
    [Prefix . pUnary . choice $ [lexNeg, lexNegU]]
  ]

pTerm :: ParserF Expr
pTerm = choice [try pArray, pTerm']

-- To avoid stuck at parsing terms other than array
pTerm' :: ParserF Expr
pTerm' = choice [pParen, pVar, pConst, pLit, pQuant] <?> "term"

pParen :: ParserF Expr
pParen = Paren <$> lexParenStart <*> pExpr' <*> lexParenEnd

-- Allow A[A[i]]
pArray :: ParserF Expr
pArray = Arr <$> pTerm' <*> lexBracketStart <*> pTerm <*> lexBracketEnd

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

pSepBy ::
  ParserF (Token sep) ->
  ParserF a ->
  ParserF (SepBy sep a)
pSepBy delim p = do
  x <- p
  let f = return (Head x)
  let g = Delim x <$> delim <*> pSepBy delim p
  try g <|> f

pList :: ParserF a -> ParserF (SepBy tokComma a)
pList = pSepBy lexComma

pIfoDoHelper ::
  ParserF (Token s) ->
  ParserF (Token e) ->
  Parser (Token s, SepBy sep GdCmd, Token e)
pIfoDoHelper start end =
  pIndentSepBy start end (pGdCmd, lexGuardBar)

------------------------------------------
-- Dirty Indents
------------------------------------------

pBlock ::
  ParserF (Token s) ->
  ParserF (Token e) ->
  Parser a ->
  Parser (Token s, [a], Token e)
pBlock start end p = do
  ref <- Lex.indentLevel
  ts <- (↓) start sc
  (t, te) <- manyTill_ p ((↓) end sc)
  let pos = getTokenColumn te
  if compare pos ref == Ord.LT
    then Lex.incorrectIndent Ord.EQ pos ref
    else return (ts, t, te)

pIndentSepBy ::
  ParserF (Token s) ->
  ParserF (Token e) ->
  (ParserF a, ParserF (Token sep)) ->
  Parser (Token s, SepBy sep a, Token e)
pIndentSepBy start end (p, delim) = do
  ref <- Lex.indentLevel
  -- parse start token and guard the indentation level
  ts <- (↓) start sc

  -- start parsing p
  ps <- parseP ref

  -- guard the end token position
  indentGTE ref scn
  te <- (↓) end sc

  return (ts, ps, te)
  where
    parseP ref = do
      gdPos <- Lex.indentGuard scn Ord.GT ref
      x <- (↓) p . void $ Lex.indentGuard scn Ord.GT ref
      let g = do
            delimPos <- Lex.indentGuard scn Ord.GT ref
            if compare delimPos gdPos == Ord.LT
              then Delim x <$> (↓) delim sc <*> parseP' gdPos delimPos ref
              else Lex.incorrectIndent Ord.LT gdPos delimPos
      try g <|> return (Head x)

    parseP' gdPos delimPos ref = do
      void $ Lex.indentGuard sc Ord.EQ gdPos
      x <- (↓) p . void $ Lex.indentGuard scn Ord.GT ref
      let g = do
            void $ Lex.indentGuard scn Ord.EQ delimPos
            Delim x <$> (↓) delim sc <*> parseP' gdPos delimPos ref
      try g <|> return (Head x)

indentedItems ::
  (MonadParsec e s m) =>
  Pos ->
  Pos ->
  m () ->
  m b ->
  m [b]
indentedItems ref lvl sc' p = go
  where
    go = do
      sc'
      pos <- Lex.indentLevel
      done <- isJust <$> optional eof
      if done
        then return []
        else
          if
              | pos <= ref -> return []
              | pos == lvl -> (:) <$> p <*> go
              | otherwise -> Lex.incorrectIndent Ord.EQ lvl pos

indentGTE :: Pos -> Parser () -> Parser ()
indentGTE ref sc' = void $ try (Lex.indentGuard sc' Ord.EQ ref) <|> Lex.indentGuard sc' Ord.GT ref