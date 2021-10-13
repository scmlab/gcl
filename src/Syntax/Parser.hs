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
  pDeclarationF = lift pDeclaration
  wrap          = do
    choice
      [ (\x -> ([x], [], [])) <$> pDeclarationF
      , (\x -> ([], [x], [])) <$> pDefinitionBlockF
      , (\x -> ([], [], [x])) <$> lift pStmt
      ]

------------------------------------------
-- parse Declaration
------------------------------------------
--
pDeclaration :: Parser Declaration
pDeclaration = Lex.lineFold scn (unParseFunc p)
  where p = (pConstDeclF <|> pVarDeclF) <* lift scn <?> "declaration"

pDefinitionBlock :: Parser DefinitionBlock
pDefinitionBlock = unParseFunc pDefinitionBlockF scn

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
pDefinitionF = lift $ Lex.lineFold
  scn
  (unParseFunc (choice [try pFuncDefnSigF, pTypeDefnF, pFuncDefnF]))
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

pStmts :: Parser [Stmt]
pStmts = unParseFunc pStmtsF scn
  -- (↓) (pIndentBlockF (lift pStmt)) scn <|> return []

pStmtsF :: ParserF [Stmt]
pStmtsF = pIndentBlockF (lift pStmt)

-- NOTE :: this function doesn't consume newline after finish parsing the statement
pStmt :: Parser Stmt
pStmt = Lex.lineFold scn (unParseFunc pStmtF) <?> "statement"
 where
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
      <*  lift sc
      <?> "statement"

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

  pDoF     = Do <$> lexDoF <*> pIndentSepByF pGdCmdF lexGuardBarF <*> lexOdF

  pIfF     = If <$> lexIfF <*> pIndentSepByF pGdCmdF lexGuardBarF <*> lexFiF

  pGdCmdF  = GdCmd <$> pExprF <*> lexArrowF <*> pIndentBlockF (lift pStmt)

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

pType :: Parser Type
pType = unParseFunc pTypeF scn <?> "type"

pTypeF :: ParserF Type
pTypeF =
  makeExprParser pTypeFTerm [[InfixR pTFuncF]]
    <*  ParseFunc (\sc' -> try sc' <|> sc)
    <?> "type"
 where
  pTypeFTerm = choice [pTParenF, pTArrayF, try pTVarF, pTConF]

  pTFuncF    = do
    arrow <- lexArrowF
    return $ \t1 t2 -> TFunc t1 arrow t2

  pTParenF   = TParen <$> lexParenStartF <*> pTypeF <*> lexParenEndF

  pTArrayF   = TArray <$> lexArrayF <*> pIntervalF <*> lexOfF <*> pTypeF

  pTConF     = TCon <$> lexNameF <*> many lexNameF

  pTVarF     = TVar <$> lexAnyNameF <* notFollowedBy lexAnyNameF

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

pExpr :: Parser Expr
pExpr = unParseFunc pExprF scn <?> "expression"

pExprF :: ParserF Expr
pExprF =
  makeExprParser pExprArithF chainOpTable
    <*  ParseFunc (\sc' -> try sc' <|> sc)
    <?> "expression"
 where
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

  arithTable =
    [ [InfixL pAppF]
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

  pExprArithF =
    makeExprParser pTermF arithTable <* ParseFunc (\sc' -> try sc' <|> sc)

  -- To avoid stuck at parsing terms other than array
  pTermF =
    choice [pCaseOfF, pLitF, try pArrayF, pParenF, pVarF, pConstF, pQuantF]
      <?> "term"

  pParenF = Paren <$> lexParenStartF <*> pExprF <*> lexParenEndF

  -- Allow A[A[i]], A[i1][i2]...[in]
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

  pLitF   = Lit <$> lexLitsF

  pVarF   = Var <$> lexLowerNameF

  pConstF = Const <$> lexNameF

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

  pAppF = pure App

  pBinaryF m = do
    -- NOTE: operator cannot be followed by any symbol
    op <- try (notFollowedBySymbolF m)
    return $ \x y -> App (App (Op op) x) y

  pUnaryF m = do
    -- NOTE: operator cannot be followed by any symbol
    op <- try (notFollowedBySymbolF m)
    return $ \x -> App (Op op) x

  pCaseOfF =
    Case <$> lexCase <*> pExprF <*> lexOfF <*> pIndentBlockF pCaseConstructorF

  pCaseConstructorF = ParseFunc $ sc' ->
    Lex.lineFold sc'
      .   unParseFunc
      $   CaseConstructor
      <$> lexUpperNameF
      <*> many lexLowerNameF
      <*> lexArrowF
      <*> pExprF

------------------------------------------
-- Pattern matching
------------------------------------------

pPattern :: Parser Pattern
pPattern = unParseFunc pPatternF scn <?> "pattern"

pPatternF :: ParserF Pattern
pPatternF = choice
  [ PattParen <$> lexParenStartF <*> pPatternF <*> lexParenEndF
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
pIndentSepByF
  :: ParserF a
  ->                      -- parser to be indented
     ParserF (Token sep)
  ->            -- delim parser
     ParserF (SepBy sep a)
pIndentSepByF p delim = do
  gdPos <- Lex.indentLevel
  x     <- p
  let g = do
        delimPos <- Lex.indentLevel
        if compare delimPos gdPos == Ord.LT
          then Delim x <$> lift (delimF gdPos) <*> parseP gdPos delimPos
          else Lex.incorrectIndent Ord.LT gdPos delimPos
  try g <|> return (Head x)
 where
    -- make sure parser after delim start at the same position
  delimF pos = unParseFunc delim (void $ Lex.indentGuard sc Ord.EQ pos)

  parseP gdPos delimPos = do
    x <- p
    let g = do
          -- make sure the delim parser start at the same position
          lift . void $ Lex.indentGuard scn Ord.EQ delimPos
          Delim x <$> lift (delimF gdPos) <*> parseP gdPos delimPos
    try g <|> return (Head x)

pIndentBlockF
  :: ParserF a
  ->                      -- parser to be indented
     ParserF [a]
pIndentBlockF p = do
  pos <- Lex.indentLevel
  indentedItems pos (lift scn) p

-- copied from Text.Megaparsec.Char.Lexer
indentedItems :: (MonadParsec e s m) => Pos -> m () -> m b -> m [b]
indentedItems lvl sc' p = go
 where
  go = do
    sc'
    pos  <- Lex.indentLevel
    done <- isJust <$> optional eof
    if not done && pos == lvl
      then try ((:) <$> p <*> go) <|> return []
      else return []
