{-# LANGUAGE DataKinds #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PartialTypeSignatures #-}

module Syntax.Parser where

import Control.Applicative.Combinators (choice, eitherP, many, optional, sepBy1, (<|>), manyTill)
import Control.Monad (void)
import Control.Monad.Combinators.Expr (Operator (..), makeExprParser)
import Control.Monad.Trans (lift)
import Data.Data (Proxy (Proxy))
import Data.Loc (Located (locOf))
import Data.Maybe (isJust)
import qualified Data.Ord as Ord
import Data.Text (Text)
import Syntax.Common (Name (..), Op (..))
import Syntax.Concrete (BlockDecl (..), BlockDeclaration (..), Decl (..), DeclProp (..), DeclBody (..), Declaration (..), EndpointClose (..), EndpointOpen (..), Expr (..), GdCmd (..), Interval (..), Program (..), SepBy (..), Stmt (..), TBase (..), Token (..), Type (..))
import Syntax.Concrete.Located ()
import Syntax.Parser.Lexer
import Syntax.Parser.Util
import Text.Megaparsec (MonadParsec (..), Pos, anySingle, mkPos, parse, tokensToChunk, unPos, (<?>))
import Text.Megaparsec.Char (eol)
import qualified Text.Megaparsec.Char.Lexer as Lex

-- The monad binding of ParserF will insert space consumer or indent guard inbetween, 
-- which sould be convenient for handling linefold indentation. 

-- Therefore, users are suggested to implement low level parsers, helper functions 
-- under ParserF monad. For the sake of need not bother handling indentation for linefold, 
-- which should be left for top level parsers to handle.

-- Hence, the Parser monad is only restricted to top level parsers 
-- (e.g. pProgram, pDeclaration, pBlockDeclaration, pStmts, pStmt, pExpr, pType ...)

-- In some case, we may want to release the restriction of linefold under the ParserF monad,
-- which can be achieve by `lift p` (p : Parser a), see `pBlock` for example.

-- While we may want to do the opposite way under Parser monad in order to 
-- specify the space consumer that we wanted to use, which can be achive by using the 
-- downward coercion combinater `(↓) p sc` (p : ParserF a), see `pBlockDeclaration` for example.

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
    p = choice
        [ try pConstDeclWithProp,
          pConstDecl,
          try pVarDeclWithProp,
          pVarDecl,
          pLetDecl
        ]
        <* lift scn
        <?> "declaration"

pBlockDeclaration :: Parser BlockDeclaration
pBlockDeclaration = (↓) pBlockDeclaration' scn

pBlockDeclaration' :: ParserF BlockDeclaration
pBlockDeclaration' =
  BlockDeclaration
  <$> lexDeclStart
  <*> pIndentBlock pBlockDecl True
  <*> lexDeclEnd

pConstDecl :: ParserF Declaration
pConstDecl =
  ConstDecl
  <$> lexCon
  <*> pDecl pName

pConstDeclWithProp :: ParserF Declaration
pConstDeclWithProp =
  ConstDeclWithProp
  <$> lexCon
  <*> pDecl pName
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
  <*> pDeclBody

pDecl :: ParserF Name -> ParserF Decl
pDecl name =
  Decl
  <$> pList name
  <*> lexColon
  <*> pType'

pDeclProp :: ParserF DeclProp
pDeclProp =
  DeclProp
  <$> lexBraceStart
  <*> pExpr'
  <*> lexBraceEnd

pDeclBody :: ParserF DeclBody
pDeclBody =
  DeclBody
  <$> pName
  <*> many lowerName
  <*> lexEQ'
  <*> pExpr'

pBlockDecl :: ParserF BlockDecl
pBlockDecl = do
  ref <- Lex.indentLevel
  (decl, mDeclProp) <- lift $ Lex.lineFold scn (constDecl ↓)
  mDeclBody <- (try . optional) (declBodyGuard ref >> pDeclBody) <|> return Nothing
  return $ BlockDecl decl mDeclProp mDeclBody
  where
    constDecl = (,) <$> pDecl pName <*> optional (eitherP pDeclProp pExpr')
    declBodyGuard ref = lift $ Lex.indentGuard scn Ord.EQ ref


------------------------------------------
-- parse Stmt
------------------------------------------

pStmts :: Parser [Stmt]
pStmts = many (pStmt <* scn) <?> "statements"

-- NOTE :: this function doesn't consume newline after finish parsing the statement
pStmt :: Parser Stmt
pStmt = Lex.lineFold scn (pStmt' ↓) <?> "statement"

pStmt' :: ParserF Stmt
pStmt' =
  choice
    [ pSkip,
      pAbort,
      try pAssert,
      pLoopInvariant,
      pAssign,
      pDo,
      pIf,
      pSpecQM,
      pSpec,
      pProof
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

pDo :: ParserF Stmt
pDo = 
  Do
  <$> lexDo
  <*> pIndentSepBy pGdCmd lexGuardBar 
  <*> lexOd

pIf :: ParserF Stmt
pIf = do
  If
  <$> lexIf
  <*> pIndentSepBy pGdCmd lexGuardBar 
  <*> lexFi

pGdCmd :: ParserF GdCmd
pGdCmd =
  GdCmd
  <$> pExpr'
  <*> lexArrow
  <*> pIndentBlock (lift pStmt) False

pSpecQM :: ParserF Stmt
pSpecQM = SpecQM . locOf <$> lexQM

pSpec :: ParserF Stmt
pSpec = do
  (ts, t, te) <- pBlock lexSpecStart anySingle lexSpecEnd
  return $ Spec ts (tokensToChunk (Proxy :: Proxy Text) t) te

pProof :: ParserF Stmt
pProof = do
  (ts, _, te) <- pBlock lexProofStart anySingle lexProofEnd
  return $ Proof ts te

------------------------------------------
-- parse Type
------------------------------------------

pType :: Parser Type
pType = (↓) pType' scn <?> "type"

pType' :: ParserF Type
pType' = makeExprParser pType'Term [[InfixR pFunction]]
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
pConst = Const <$> pName

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

pName :: ParserF Name
pName = uncurry Name <$> lexText

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

------------------------------------------
-- Dirty Indents
------------------------------------------

-- parse a block regardless of indentation
-- e.g.
--    start
--  p0
--            p1
--      p2
--  ...
--    end
-- 
pBlock ::
  ParserF s ->                      -- start parser
  ParserF a ->                      -- parser inbetween
  ParserF e ->                      -- end parser
  ParserF (s, [a], e)
pBlock start p end = do
  ts <- lift start'                        -- release linefold restriction 
  t <- lift $ manyTill p' (lookAhead end') -- release linefold restriction
  te <- end
  return (ts, t, te)
  where
    p' = (↓) p sc
    start' = (↓) start sc
    end' = (↓) end sc

-- parse indentblock with delim
pIndentSepBy ::
  ParserF a ->                      -- parser to be indented
  ParserF (Token sep) ->            -- delim parser
  ParserF (SepBy sep a)
pIndentSepBy p delim = do
  gdPos <- Lex.indentLevel
  x <- p
  let g = do
        delimPos <- Lex.indentLevel
        if compare delimPos gdPos == Ord.LT
          then Delim x <$> lift (delim' gdPos) <*> parseP gdPos delimPos
          else Lex.incorrectIndent Ord.LT gdPos delimPos
  try g <|> return (Head x)
  where
    -- make sure parser after delim start at the same position
    delim' pos = (↓) delim (void $ Lex.indentGuard sc Ord.EQ pos)   

    parseP gdPos delimPos = do
      x <- p
      let g = do
            -- make sure the delim parser start at the same position
            lift . void $ Lex.indentGuard scn Ord.EQ delimPos         
            Delim x <$> lift (delim' gdPos) <*> parseP gdPos delimPos
      try g <|> return (Head x)


pIndentBlock ::
  ParserF a ->                      -- parser to be indented
  Bool ->                           -- restriction of indentation, 
                                    -- False if indented parser can have the same level as reference parser (e.g. GdCmd)
                                    -- True if the parser have to be strictly indented
  ParserF [a]
pIndentBlock p restrict = do
  pos <- Lex.indentLevel
  p0 <- lift p'

  isEol <- optional . try . lift $ eol
  done <- isJust <$> optional eof
  case (isEol, done) of
    (Just _, False) -> do
      let ref = if restrict
          then pos
          else posMoveLeft pos 1
      ps <- lift $ indentedItems ref pos scn p'
      return (p0 : ps)
    _ -> return [p0]                          -- eof or no newline => only one indented element
  where
    p' = (↓) p sc                             -- make sure p doesn't parse newline
    posMoveLeft pos i = mkPos (unPos pos - i) -- safe, since pos should be greater than ref,
                                              -- by the definition of ParserF 

-- copied from Text.Megaparsec.Char.Lexer
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