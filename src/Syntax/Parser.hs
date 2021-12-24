{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PartialTypeSignatures #-}

module Syntax.Parser where

import           Control.Applicative.Combinators
                                                ( (<|>)
                                                , choice
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
import           Data.Data                      ( Proxy(Proxy) )
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
-- which can be achieve by `lift p` (p : Parser a), see `pBlockTextF` for example.

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
  unParseFunc (pProgramF <* eof) scn

pProgramF :: ParserF Program
pProgramF = do
  (x, y, z) <- mconcat <$> pIndentBlockF wrap
  return $ Program (map Left x <> map Right y) z
 where
  wrap = do
    choice
      [ (\x -> ([x], [], [])) <$> pDeclarationF
      , (\x -> ([], [x], [])) <$> pDefinitionBlockF
      , (\x -> ([], [], [x])) <$> pStmtF
      ]

------------------------------------------
-- parse Declaration
------------------------------------------
--
pDeclarationF :: ParserF Declaration
pDeclarationF = pConstDeclF <|> pVarDeclF

pDefinitionBlockF :: ParserF DefinitionBlock
pDefinitionBlockF =
  DefinitionBlock
    <$> lexDeclStartF
    <*> pIndentBlockF pDefinitionF
    <*> lexDeclEndF

pConstDeclF :: ParserF Declaration
pConstDeclF = ConstDecl <$> lexConF <*> pDeclTypeF lexNameF

pVarDeclF :: ParserF Declaration
pVarDeclF = VarDecl <$> lexVarF <*> pDeclTypeF lexLowerNameF

-- `T a1 a2 ... = C1 ai1 ai2 .. | C2 ... | ...`
pTypeDefnF :: ParserF Definition
pTypeDefnF =
  TypeDefn
    <$> lexDataF
    <*> lexNameF
    <*> many lexNameF
    <*> lexEqualF
    <*> pSepByF lexGuardBarF pTypeDefnCtorF

pTypeDefnCtorF :: ParserF TypeDefnCtor
pTypeDefnCtorF = TypeDefnCtor <$> lexNameF <*> many pTypeF

-- `n : type` | `n : type { expr }` | `T a1 a2 ... = C1 ai1 ai2 .. | C2 ... | ...` | `n args = expr`
pDefinitionF :: ParserF Definition
pDefinitionF = choice [try pFuncDefnSigF, pTypeDefnF, pFuncDefnF]
 where
  pFuncDefnSigF = FuncDefnSig <$> pDeclBaseF lexNameF <*> optional pDeclPropF
  pFuncDefnF =
    FuncDefn <$> lexNameF <*> many lexLowerNameF <*> lexEqualF <*> pExprF

-- `n : type`
pDeclBaseF :: ParserF Name -> ParserF DeclBase
pDeclBaseF name = DeclBase <$> pListF name <*> lexColonF <*> pTypeF

-- `{ expr }`
pDeclPropF :: ParserF DeclProp
pDeclPropF = DeclProp <$> lexBraceStartF <*> pExprF <*> lexBraceEndF

-- `n : type` | `n : type { expr }`
pDeclTypeF :: ParserF Name -> ParserF DeclType
pDeclTypeF name = DeclType <$> pDeclBaseF name <*> optional pDeclPropF

------------------------------------------
-- parse Stmt
------------------------------------------

-- NOTE :: this function doesn't consume newline after finish parsing the statement
pStmtF :: ParserF Stmt
pStmtF =
  choice
      [ pSkipF
      , pProofF
      , pAbortF
      , try pAssertF
      , pLoopInvariantF
      , try pAssignF
      , try pAAssignF
      , try pAllocF
      , try pHLookupF
      , pHMutateF
      , pDisposeF
      , pDoF
      , pIfF
      , pSpecQMF
      , pSpecF
      , pBlockF
      ]
    <?> "statement"

 where
  pSkipF   = Skip . rangeOf <$> lexSkipF

  pAbortF  = Abort . rangeOf <$> lexAbortF

  pAssignF = Assign <$> pListF lexLowerNameF <*> lexAssignF <*> pListF pExprF

  pAAssignF =
    AAssign
      <$> lexLowerNameF
      <*> lexBracketStartF
      <*> pExprF
      <*> lexBracketEndF
      <*> lexAssignF
      <*> pExprF

  pAssertF = Assert <$> lexBraceStartF <*> pExprF <*> lexBraceEndF

  pLoopInvariantF =
    LoopInvariant
      <$> lexBraceStartF
      <*> pExprF
      <*> lexCommaF
      <*> lexBndF
      <*> lexColonF
      <*> pExprF
      <*> lexBraceEndF

  pDoF =
    Do
      <$> lexDoF
      <*> pIndentSepByF pGdCmdF lexGuardBarF
      <*  optional eol
      <*> lexOdF

  pIfF =
    If
      <$> lexIfF
      <*> pIndentSepByF pGdCmdF lexGuardBarF
      <*  optional eol
      <*> lexFiF

  pGdCmdF  = GdCmd <$> pExprF <*> lexArrowF <*> pIndentBlockF pStmtF

  pSpecQMF = SpecQM . rangeOf <$> lexQMF

  pSpecF   = do
    (ts, t, te) <- pBlockTextF lexSpecStartF anySingle lexSpecEndF
    return $ Spec ts (tokensToChunk (Proxy :: Proxy Text) t) te

  pProofAnchorF = uncurry ProofAnchor <$> lexProofAnchorF

  pProofF =
    Proof <$> lexProofStartF <*> pProofAnchorsOrProofEnd <*> lexProofEndF

  pProofAnchorsOrProofEnd = do
    let anchorOrEnd =
          choice [lexProofEndF >> return False, pProofAnchorF >> return True]
    (_, continue) <- manyTill_ anySingle (lookAhead anchorOrEnd)
    if continue
      then do
        x  <- pProofAnchorF
        xs <- pProofAnchorsOrProofEnd
        return (x : xs)
      else return []

  pAllocF =
    Alloc
      <$> lexLowerNameF
      <*> lexAssignF
      <*> lexNewF
      <*> lexParenStartF
      <*> pListF pExprF
      <*> lexParenEndF

  pHLookupF = HLookup <$> lexLowerNameF <*> lexAssignF <*> lexStarF <*> pExprF

  pHMutateF = HMutate <$> lexStarF <*> pExprF <*> lexAssignF <*> pExprF

  pDisposeF = Dispose <$> lexDisposeF <*> pExprF

  pBlockF   = Block <$> lexBlockStartF <*> pProgramF <*> lexBlockEndF

------------------------------------------
-- parse Type
------------------------------------------

pTypeF :: ParserF Type
pTypeF = makeExprParser pTypeFTerm [[InfixR pTFuncF]] <?> "type"
 where
  pTypeFTerm = choice [pTParenF, pTArrayF, try pTVarF, pTConF]

  pTFuncF    = do
    arrow <- lexArrowF
    return $ \t1 t2 -> TFunc t1 arrow t2

  pTParenF   = TParen <$> lexParenStartF <*> pTypeF <*> lexParenEndF

  pTArrayF   = TArray <$> lexArrayF <*> pIntervalF <*> lexOfF <*> pTypeF

  pTConF     = TCon <$> lexNameF <*> many lexNameF

  pTVarF     = TVar <$> lexAnyNameF <* notFollowedBy lexNameF

  pIntervalF = Interval <$> pEndpointOpenF <*> lexRangeF <*> pEndpointCloseF

  pEndpointOpenF =
    (IncludingOpening <$> lexBracketStartF <*> pExprF)
      <|> (ExcludingOpening <$> lexParenStartF <*> pExprF)

  pEndpointCloseF =
    try (IncludingClosing <$> pExprF <*> lexBracketEndF)
      <|> (ExcludingClosing <$> pExprF <*> lexParenEndF)

--------------------------------------------------------------------------------
-- | Expressions
--------------------------------------------------------------------------------

pExprF :: ParserF Expr
pExprF =
  makeExprParser (pExprArithF <|> pCaseOfF) chainOpTable <?> "expression"
 where
  chainOpTable :: [[Operator ParserF Expr]]
  chainOpTable =
    [ [InfixL . pBinaryF . fmap ChainOp $ lexEQF]
    , [ InfixL . pBinaryF . fmap ChainOp . choice $ [lexNEQF, lexNEQUF]
      , InfixL . pBinaryF . fmap ChainOp $ lexLTF
      , InfixL . pBinaryF . fmap ChainOp . choice $ [lexLTEF, lexLTEUF]
      , InfixL . pBinaryF . fmap ChainOp $ lexGTF
      , InfixL . pBinaryF . fmap ChainOp . choice $ [lexGTEF, lexGTEUF]
      ]
    , [InfixL . pBinaryF . fmap ArithOp . choice $ [lexConjF, lexConjUF]]
    , [InfixL . pBinaryF . fmap ArithOp . choice $ [lexDisjF, lexDisjUF]]
    , [InfixL . pBinaryF . fmap ArithOp . choice $ [lexImplF, lexImplUF]]
    , [ InfixL . pBinaryF . fmap ChainOp $ lexEQPropF
      , InfixL . pBinaryF . fmap ChainOp $ lexEQPropUF
      ]
    ]

  arithTable :: [[Operator ParserF Expr]]
  arithTable =
    [ [InfixL pAppF]
    , [Prefix . pUnaryF . fmap ArithOp $ lexNegNumF]
    , [InfixN . pBinaryF . fmap ArithOp $ lexExpF]
    , [ InfixN . pBinaryF . fmap ArithOp $ lexMaxF
      , InfixN . pBinaryF . fmap ArithOp $ lexMinF
      ]
    , [InfixL . pBinaryF . fmap ArithOp $ lexModF]
    , [ InfixL . pBinaryF . fmap ArithOp $ lexMulF
      , InfixL . pBinaryF . fmap ArithOp $ lexDivF
      ]
    , [ InfixL . pBinaryF . fmap ArithOp $ lexAddF
      , InfixL . pBinaryF . fmap ArithOp $ lexSubF
      ]
    , [Prefix . pUnaryF . fmap ArithOp . choice $ [lexNegF, lexNegUF]]
    ]

  pExprArithF :: ParserF Expr
  pExprArithF = makeExprParser pTermF arithTable

  -- To avoid stuck at parsing terms other than array
  pTermF :: ParserF Expr
  pTermF =
    choice [pLitF, try pArrayF, pParenF, pVarF, pConstF, pQuantF] <?> "term"

  pParenF :: ParserF Expr
  pParenF = Paren <$> lexParenStartF <*> pExprF <*> lexParenEndF

  -- Allow A[A[i]], A[i1][i2]...[in]
  pArrayF :: ParserF Expr
  pArrayF = do
    arr <- choice [pParenF, pVarF, pConstF]
    is  <- some $ do
      bs <- lexBracketStartF
      i  <- pExprArithF
      be <- lexBracketEndF
      return (bs, i, be)
    return $ pArrayHelper arr is
   where
    pArrayHelper :: Expr -> [(Token "[", Expr, Token "]")] -> Expr
    pArrayHelper a []                 = a
    pArrayHelper a ((bs, i, be) : is) = pArrayHelper (Arr a bs i be) is

  pLitF :: ParserF Expr
  pLitF = Lit <$> lexLitsF

  pVarF :: ParserF Expr
  pVarF = Var <$> lexLowerNameF

  pConstF :: ParserF Expr
  pConstF = Const <$> lexNameF

  pQuantF :: ParserF Expr
  pQuantF =
    Quant
      <$> lexQuantStartsF
      <*> pQuantOp
      <*> pQuantNames
      <*> lexColonF
      <*> pExprF
      <*> lexColonF
      <*> pExprF
      <*> lexQuantEndsF
   where
    pQuantOp    = choice [Left <$> lexOpsF, Right <$> pTermF]
    pQuantNames = sepBy1 lexLowerNameF . try . ParseFunc $ id

  pAppF :: ParserF (Expr -> Expr -> Expr)
  pAppF = pure App

  pBinaryF :: ParserF Op -> ParserF (Expr -> Expr -> Expr)
  pBinaryF m = do
    -- NOTE: operator cannot be followed by any symbol
    op <- try (notFollowedBySymbolF m)
    return $ \x y -> App (App (Op op) x) y

  pUnaryF :: ParserF Op -> ParserF (Expr -> Expr)
  pUnaryF m = do
    -- NOTE: operator cannot be followed by any symbol
    op <- try (notFollowedBySymbolF m)
    return $ \x -> App (Op op) x

  pCaseOfF :: ParserF Expr
  pCaseOfF =
    Case <$> lexCase <*> pExprF <*> lexOfF <*> pIndentBlockF pCaseClauseF

  pCaseClauseF :: ParserF CaseClause
  pCaseClauseF = CaseClause <$> pPatternF <*> lexArrowF <*> pExprF

------------------------------------------
-- Pattern matching
------------------------------------------

pPatternF :: ParserF Pattern
pPatternF = choice
  [ PattLit <$> lexLitsF
  , PattParen <$> lexParenStartF <*> pPatternF <*> lexParenEndF
  , PattWildcard <$> lexUnderscore
  , PattBinder <$> lexLowerNameF
  , PattConstructor <$> lexUpperNameF <*> many pPatternF
  ]

------------------------------------------
-- combinators
------------------------------------------

pSepByF :: ParserF (Token sep) -> ParserF a -> ParserF (SepBy sep a)
pSepByF delim p = do
  x <- p
  let f = return (Head x)
  let g = Delim x <$> delim <*> pSepByF delim p
  try g <|> f

pListF :: ParserF a -> ParserF (SepBy tokComma a)
pListF = pSepByF lexCommaF

pScGuard :: ParserF ()
pScGuard = ParseFunc id

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
pBlockTextF
  :: ParserF s
  ->                      -- start parser
     ParserF a
  ->                      -- parser inbetween
     ParserF e
  ->                      -- end parser
     ParserF (s, [a], e)
pBlockTextF start p end = do
  ts <- lift start'                        -- release linefold restriction
  t  <- lift $ manyTill p' (lookAhead end') -- release linefold restriction
  te <- end
  return (ts, t, te)
 where
  p'     = unParseFunc p sc
  start' = unParseFunc start sc
  end'   = unParseFunc end sc

-- parse indentblock with delim
-- will not parse the last newline
pIndentSepByF :: ParserF a -> ParserF (Token sep) -> ParserF (SepBy sep a)
pIndentSepByF p delim = ParseFunc go
 where
  go sc' = do
    x <- unParseFunc p sc'

    let g = do
          delimPos <- sc' >> Lex.indentLevel
          Delim x
            <$> unParseFunc delim (void $ Lex.indentGuard sc' Ord.GT delimPos)
            <*> go sc'
    try g <|> return (Head x)

-- will not parse the last newline
pIndentBlockF
  :: ParserF a
  ->                      -- parser to be indented
     ParserF [a]
pIndentBlockF p = ParseFunc
  (\_ -> do
    pos <- Lex.indentLevel
    indentedItems pos scn (lineFold (unParseFunc p))
  )

-- modified from Text.Megaparsec.Char.Lexer
-- will not parse the last newline
indentedItems :: (MonadParsec e s m) => Pos -> m () -> m b -> m [b]
indentedItems lvl sc' p = go
 where
  go = do
    --pos  <- try (Lex.indentGuard sc' Ord.EQ lvl) <|> Lex.indentLevel
    sc'
    pos  <- Lex.indentLevel
    done <- isJust <$> optional eof
    if not done && pos == lvl then (:) <$> p <*> go <|> return [] else return []

lineFold :: (Parser () -> Parser a) -> Parser a
lineFold action = do
  Lex.indentLevel >>= action . void . Lex.indentGuard scn Ord.GT

------------------------------------------
-- For test only
------------------------------------------

pDeclaration :: Parser Declaration
pDeclaration = Lex.lineFold scn (unParseFunc pDeclarationF)

pDefinitionBlock :: Parser DefinitionBlock
pDefinitionBlock = scn *> unParseFunc pDefinitionBlockF scn

pStmts :: Parser [Stmt]
pStmts = scn *> unParseFunc pStmtsF scn where pStmtsF = pIndentBlockF pStmtF

pStmt :: Parser Stmt
pStmt = Lex.lineFold scn (unParseFunc pStmtF) <?> "statement"

pType :: Parser Type
pType = scn *> unParseFunc pTypeF scn <?> "type"

pExpr :: Parser Expr
pExpr = scn *> unParseFunc pExprF scn <?> "expression"

pPattern :: Parser Pattern
pPattern = scn *> unParseFunc pPatternF scn <?> "pattern"
