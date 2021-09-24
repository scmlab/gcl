{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PartialTypeSignatures #-}

module Syntax.Parser where

import           Control.Applicative.Combinators
                                                ( (<|>)
                                                , choice
                                                , eitherP
                                                , many
                                                , manyTill
                                                , optional
                                                , sepBy1
                                                , some
                                                )
import           Control.Monad                  ( void )
import           Control.Monad.Combinators.Expr ( Operator(..)
                                                , makeExprParser
                                                )
import           Control.Monad.Trans            ( lift )
import           Data.Bifunctor                 ( second )
import           Data.Data                      ( Proxy(Proxy) )
import           Data.Loc                       ( Located(locOf) )
import           Data.Loc.Range                 ( rangeOf )
import           Data.Maybe                     ( isJust )
import qualified Data.Ord                      as Ord
import           Data.Text                      ( Text )
import           Syntax.Common                  ( Name(..)
                                                , Op(..)
                                                )
import           Syntax.Concrete
import           Syntax.Parser.Lexer
import           Syntax.Parser.Util
import           Text.Megaparsec                ( (<?>)
                                                , MonadParsec(..)
                                                , Pos
                                                , anySingle
                                                , manyTill_
                                                , parse
                                                , tokensToChunk
                                                )
import           Text.Megaparsec.Char           ( eol )
import qualified Text.Megaparsec.Char.Lexer    as Lex

-- The monad binding of ParserF will insert space consumer or indent guard inbetween,
-- which sould be convenient for handling linefold indentation.

-- Therefore, users are suggested to implement low level parsers, helper functions
-- under ParserF monad. For the sake of need not bother handling indentation for linefold,
-- which should be left for top level parsers to handle.

-- Hence, the Parser monad is only restricted to toppest level parsers
-- (e.g. pProgram, pDeclaration, pBlockDeclaration, pStmt, pStmts, pExpr, pType ...)

-- In some case, we may want to release the restriction of linefold under the ParserF monad,
-- which can be achieve by `lift p` (p : Parser a), see `pBlockText` for example.

-- While we may want to do the opposite way under Parser monad in order to
-- specify the space consumer that we wanted to use, which can be achive by using the
-- downward coercion combinater `(↓) p sc` (p : ParserF a), see `pDefinitions` for example.

type Parser = Lexer

type ParserF = LexerF

------------------------------------------
-- parse Program
------------------------------------------

runParse :: Parser a -> FilePath -> Text -> Either [SyntacticError] a
runParse p filepath s = case parse p filepath s of
  Left  e -> Left (fromParseErrorBundle e)
  Right x -> Right x

pProgram :: Parser Program
pProgram = do
  scn
  unParseFunc (pProgram' <* eof) scn

pProgram' :: ParserF Program
pProgram' =
  Program <$> many (eitherP pDeclaration' pDefinitionBlock') <*> pStmts'

------------------------------------------
-- parse Declaration
------------------------------------------

pDeclaration :: Parser Declaration
pDeclaration = Lex.lineFold scn (unParseFunc p)
 where
  p = (pConstDecl <|> pVarDecl) <* lift scn <?> "declaration"

pDeclaration' :: ParserF Declaration
pDeclaration' = lift pDeclaration

pDefinitionBlock :: Parser DefinitionBlock
pDefinitionBlock = unParseFunc pDefinitionBlock' scn

pDefinitionBlock' :: ParserF DefinitionBlock
pDefinitionBlock' =
  DefinitionBlock <$> lexDeclStart <*> pIndentBlock pDefinition <*> lexDeclEnd

pConstDecl :: ParserF Declaration
pConstDecl = ConstDecl <$> lexCon <*> pDeclType pName

pVarDecl :: ParserF Declaration
pVarDecl = VarDecl <$> lexVar <*> pDeclType lowerName

pTypeDefn :: ParserF Definition
pTypeDefn =
  TypeDefn
    <$> lexData
    <*> pName
    <*> many pName
    <*> lexEqual
    <*> pSepBy lexGuardBar pTypeDefnCtor

pTypeDefnCtor :: ParserF TypeDefnCtor
pTypeDefnCtor = TypeDefnCtor <$> pName <*> many pType'


pDefinition :: ParserF Definition
pDefinition = lift $ Lex.lineFold
  scn
  (unParseFunc
    (choice
      [ try $ FuncDefnSig <$> pDeclBase pName <*> optional pDeclProp
      , pTypeDefn
      , FuncDefn <$> pDeclBody
      ]
    )
  )

pDeclBase :: ParserF Name -> ParserF DeclBase
pDeclBase name = DeclBase <$> pList name <*> lexColon <*> pType'

pDeclProp :: ParserF DeclProp
pDeclProp = DeclProp <$> lexBraceStart <*> pExpr' <*> lexBraceEnd

pDeclType :: ParserF Name -> ParserF DeclType
pDeclType name = DeclType <$> pDeclBase name <*> optional pDeclProp

pDeclBody :: ParserF DeclBody
pDeclBody = DeclBody <$> pName <*> many lowerName <*> lexEqual <*> pExpr'

------------------------------------------
-- parse Stmt
------------------------------------------

pStmts :: Parser [Stmt]
pStmts = unParseFunc pStmts' scn <|> return []
---- pStmts = many (pStmt <* scn)

pStmts' :: ParserF [Stmt]
pStmts' = pIndentBlock (lift pStmt)

-- NOTE :: this function doesn't consume newline after finish parsing the statement
pStmt :: Parser Stmt
pStmt = Lex.lineFold scn (unParseFunc pStmt') <?> "statement"

pStmt' :: ParserF Stmt
pStmt' =
  choice
      [ pSkip
      , pProof
      , pAbort
      , try pAssert
      , pLoopInvariant
      , try pAssign
      , try pAAssign
      , try pAlloc
      , try pHLookup
      , pHMutate
      , pDispose
      , pDo
      , pIf
      , pSpecQM
      , pSpec
      , pBlock
      ]
    <?> "statement"

pSkip :: ParserF Stmt
pSkip = Skip . rangeOf <$> lexSkip

pAbort :: ParserF Stmt
pAbort = Abort . rangeOf <$> lexAbort

pAssign :: ParserF Stmt
pAssign = Assign <$> pList lowerName <*> lexAssign <*> pList pExpr'

pAAssign :: ParserF Stmt
pAAssign =
  AAssign
    <$> lowerName
    <*> lexBracketStart
    <*> pExpr'
    <*> lexBracketEnd
    <*> lexAssign
    <*> pExpr'

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
pDo = Do <$> lexDo <*> pIndentSepBy pGdCmd lexGuardBar <*> lexOd

pIf :: ParserF Stmt
pIf =
  do
      If
    <$> lexIf
    <*> pIndentSepBy pGdCmd lexGuardBar
    <*> lexFi

pGdCmd :: ParserF GdCmd
pGdCmd = GdCmd <$> pExpr' <*> lexArrow <*> pStmts'

pSpecQM :: ParserF Stmt
pSpecQM = SpecQM . rangeOf <$> lexQM

pSpec :: ParserF Stmt
pSpec = do
  (ts, t, te) <- pBlockText lexSpecStart anySingle lexSpecEnd
  return $ Spec ts (tokensToChunk (Proxy :: Proxy Text) t) te

pProofAnchor :: ParserF ProofAnchor
pProofAnchor = uncurry ProofAnchor <$> lexProofAnchor

pProof :: ParserF Stmt
pProof = Proof <$> lexProofStart <*> pProofAnchorsOrProofEnd <*> lexProofEnd
 where
  pProofAnchorsOrProofEnd :: ParserF [ProofAnchor]
  pProofAnchorsOrProofEnd = do
    let anchorOrEnd =
          choice [lexProofEnd >> return False, pProofAnchor >> return True]
    (_, continue) <- manyTill_ anySingle (lookAhead anchorOrEnd)
    if continue
      then do
        x  <- pProofAnchor
        xs <- pProofAnchorsOrProofEnd
        return (x : xs)
      else return []

pAlloc :: ParserF Stmt
pAlloc =
  Alloc
    <$> lowerName
    <*> lexAssign
    <*> lexNew
    <*> lexParenStart
    <*> pList pExpr'
    <*> lexParenEnd

pHLookup :: ParserF Stmt
pHLookup = HLookup <$> lowerName <*> lexAssign <*> lexStar <*> pExpr'

pHMutate :: ParserF Stmt
pHMutate = HMutate <$> lexStar <*> pExpr' <*> lexAssign <*> pExpr'

pDispose :: ParserF Stmt
pDispose = Dispose <$> lexDispose <*> pExpr'

pBlock :: ParserF Stmt
pBlock = Block <$> lexBlockStart <*> pProgram' <*> lexBlockEnd

------------------------------------------
-- parse Type
------------------------------------------

pType :: Parser Type
pType = unParseFunc pType' scn <?> "type"

pType' :: ParserF Type
pType' =
  makeExprParser pTypeTerm [[InfixR pTFunc]]
    <*  ParseFunc (\sc' -> try sc' <|> sc)
    <?> "type"
  where pTypeTerm = choice [pTParen, pTArray, pTBase, try pTVar, pTCon]

pTFunc :: ParserF (Type -> Type -> Type)
pTFunc = do
  arrow <- lexArrow
  return $ \t1 t2 -> TFunc t1 arrow t2

pTParen :: ParserF Type
pTParen = TParen <$> lexParenStart <*> pType' <*> lexParenEnd

pTArray :: ParserF Type
pTArray = TArray <$> lexArray <*> pInterval <*> lexOf <*> pType'

pTCon :: ParserF Type
pTCon = TCon <$> pName <*> many pName

pTBase :: ParserF Type
pTBase = TBase <$> choice
  [ TInt . rangeOf <$> lexTypeInt
  , TBool . rangeOf <$> lexTypeBool
  , TChar . rangeOf <$> lexTypeChar
  ]

-- distinguish with pTCon
pTVar :: ParserF Type
pTVar = TVar <$> pName <* notFollowedBy pName

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
pExpr = unParseFunc pExpr' scn <?> "expression"

pExpr' :: ParserF Expr
pExpr' =
  makeExprParser pExprArith chainOpTable
    <*  ParseFunc (\sc' -> try sc' <|> sc)
    <?> "expression"

chainOpTable :: [[Operator ParserF Expr]]
chainOpTable =
  [ [InfixL . pBinary . fmap ChainOp $ lexEQ]
  , [ InfixL . pBinary . fmap ChainOp . choice $ [lexNEQ, lexNEQU]
    , InfixL . pBinary . fmap ChainOp $ lexLT
    , InfixL . pBinary . fmap ChainOp . choice $ [lexLTE, lexLTEU]
    , InfixL . pBinary . fmap ChainOp $ lexGT
    , InfixL . pBinary . fmap ChainOp . choice $ [lexGTE, lexGTEU]
    ]
  , [InfixL . pBinary . fmap ArithOp . choice $ [lexConj, lexConjU]]
  , [InfixL . pBinary . fmap ArithOp . choice $ [lexDisj, lexDisjU]]
  , [InfixL . pBinary . fmap ArithOp . choice $ [lexImpl, lexImplU]]
  , [ InfixL . pBinary . fmap ChainOp $ lexEQProp
    , InfixL . pBinary . fmap ChainOp $ lexEQPropU
    ]
  ]

pExprArith :: ParserF Expr
pExprArith =
  makeExprParser pTerm arithTable <* ParseFunc (\sc' -> try sc' <|> sc)

arithTable :: [[Operator ParserF Expr]]
arithTable =
  [ [Postfix pApp]
  , [InfixN . pBinary . fmap ArithOp $ lexExp]
  , [ InfixN . pBinary . fmap ArithOp $ lexMax
    , InfixN . pBinary . fmap ArithOp $ lexMin
    ]
  , [InfixL . pBinary . fmap ArithOp $ lexMod]
  , [ InfixL . pBinary . fmap ArithOp $ lexMul
    , InfixL . pBinary . fmap ArithOp $ lexDiv
    ]
  , [ InfixL . pBinary . fmap ArithOp $ lexAdd
    , InfixL . pBinary . fmap ArithOp $ lexSub
    ]
  , [Prefix . pUnary . fmap ArithOp . choice $ [lexNeg, lexNegU]]
  ]

-- pTerm :: ParserF Expr
-- pTerm = choice [try pArray, pTerm']

-- To avoid stuck at parsing terms other than array
pTerm :: ParserF Expr
pTerm = choice [pLit, try pArray, pParen, pVar, pConst, pQuant] <?> "term"

pParen :: ParserF Expr
pParen = Paren <$> lexParenStart <*> pExpr' <*> lexParenEnd

-- Allow A[A[i]], A[i1][i2]...[in]
pArray :: ParserF Expr
pArray = do
  arr <- choice [pParen, pVar, pConst]
  is  <- some $ do
    bs <- lexBracketStart
    i  <- pExprArith
    be <- lexBracketEnd
    return (bs, i, be)
  return $ pArrayHelper arr is
 where
  pArrayHelper :: Expr -> [(Token "[", Expr, Token "]")] -> Expr
  pArrayHelper a []                 = a
  pArrayHelper a ((bs, i, be) : is) = pArrayHelper (Arr a bs i be) is

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
    <*> pQuantOp
    <*> pQuantNames
    <*> lexColon
    <*> pExpr'
    <*> lexColon
    <*> pExpr'
    <*> lexQuantEnds
 where
  pQuantOp    = choice [Left <$> lexOps, Right <$> pTerm]
  pQuantNames = sepBy1 lowerName . try . ParseFunc $ id

pApp :: ParserF (Expr -> Expr)
pApp = do
  terms <- many pTerm
  return $ \func -> do
    foldl App func terms

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
pName = uncurry Name . second locOf <$> lexText

upperName :: ParserF Name
upperName = uncurry Name . second locOf <$> lexUpper

lowerName :: ParserF Name
lowerName = uncurry Name . second locOf <$> lexLower

pSepBy :: ParserF (Token sep) -> ParserF a -> ParserF (SepBy sep a)
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
pBlockText
  :: ParserF s
  ->                      -- start parser
     ParserF a
  ->                      -- parser inbetween
     ParserF e
  ->                      -- end parser
     ParserF (s, [a], e)
pBlockText start p end = do
  ts <- lift start'                        -- release linefold restriction
  t  <- lift $ manyTill p' (lookAhead end') -- release linefold restriction
  te <- end
  return (ts, t, te)
 where
  p'     = unParseFunc p sc
  start' = unParseFunc start sc
  end'   = unParseFunc end sc

-- parse indentblock with delim
pIndentSepBy
  :: ParserF a
  ->                      -- parser to be indented
     ParserF (Token sep)
  ->            -- delim parser
     ParserF (SepBy sep a)
pIndentSepBy p delim = do
  gdPos <- Lex.indentLevel
  x     <- p
  let g = do
        delimPos <- Lex.indentLevel
        if compare delimPos gdPos == Ord.LT
          then Delim x <$> lift (delim' gdPos) <*> parseP gdPos delimPos
          else Lex.incorrectIndent Ord.LT gdPos delimPos
  try g <|> return (Head x)
 where
    -- make sure parser after delim start at the same position
  delim' pos = unParseFunc delim (void $ Lex.indentGuard sc Ord.EQ pos)

  parseP gdPos delimPos = do
    x <- p
    let g = do
          -- make sure the delim parser start at the same position
          lift . void $ Lex.indentGuard scn Ord.EQ delimPos
          Delim x <$> lift (delim' gdPos) <*> parseP gdPos delimPos
    try g <|> return (Head x)

pIndentBlock
  :: ParserF a
  ->                      -- parser to be indented
     ParserF [a]
pIndentBlock p = do
  pos <- Lex.indentLevel
  mp0 <- optional . try . lift $ p'

  case mp0 of
    Just p0 -> do
      isEol <- optional . try $ eol
      done  <- isJust <$> optional eof
      case (isEol, done) of
        (Just _, False) -> do
          ps <- lift $ indentedItems pos scn p'
          return (p0 : ps)
        _ -> do
          return [p0]                          -- eof or no newline => only one indented element
    Nothing -> return []
  where p' = unParseFunc p sc                             -- make sure p doesn't parse newline

-- copied from Text.Megaparsec.Char.Lexer
indentedItems :: (MonadParsec e s m) => Pos -> m () -> m b -> m [b]
indentedItems lvl sc' p = go
 where
  go = do
    sc'
    pos  <- Lex.indentLevel
    done <- isJust <$> optional eof
    if done
      then return []
      else if pos == lvl
        then try ((:) <$> p <*> go) <|> return []
        else return []
