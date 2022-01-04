{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}

module Syntax.Parser2 where

import           Control.Monad                  ( void )
import           Control.Monad.Combinators.Expr
import           Control.Monad.State            ( lift )
import           Data.Loc
import           Data.Loc.Range
import           Data.Text                      ( Text )
import qualified Data.Text                     as Text
import           Data.Void
import           Prelude                 hiding ( Ordering(..) )
import           Syntax.Common           hiding ( Fixity(..) )
import           Syntax.Concrete         hiding ( Op )
import qualified Syntax.Concrete.Types         as Expr
import           Syntax.Parser2.Lexer
import           Syntax.Parser2.Util            ( PosLog
                                                , extract
                                                , getRange
                                                , withLoc
                                                , withRange
                                                )
import qualified Syntax.Parser2.Util           as Util
import           Text.Megaparsec         hiding ( ParseError
                                                , Pos
                                                , State
                                                , Token
                                                , parse
                                                )
import qualified Text.Megaparsec               as Mega

--------------------------------------------------------------------------------

-- | States for source location bookkeeping
type Parser = ParsecT Void TokStream (PosLog Tok)

type SyntacticError = (Loc, String)

parse :: Parser a -> FilePath -> TokStream -> Either [SyntacticError] a
parse parser filepath tokenStream =
  case Util.runPosLog (runParserT parser filepath tokenStream) of
    Left  e -> Left (fromParseErrorBundle e)
    Right x -> Right x
 where
  fromParseErrorBundle
    :: ShowErrorComponent e => ParseErrorBundle TokStream e -> [SyntacticError]
  fromParseErrorBundle (ParseErrorBundle errors posState) = snd
    $ foldr f (posState, []) errors
   where
    f
      :: ShowErrorComponent e
      => Mega.ParseError TokStream e
      -> (PosState TokStream, [SyntacticError])
      -> (PosState TokStream, [SyntacticError])
    f err (initial, accum) =
      let (_, next) = reachOffset (errorOffset err) initial
      in  (next, (getLoc err, parseErrorTextPretty err) : accum)

    getLoc :: ShowErrorComponent e => Mega.ParseError TokStream e -> Loc
    -- get the Loc of all unexpected tokens
    getLoc (TrivialError _ (Just (Tokens xs)) _) = foldMap locOf xs
    getLoc _ = mempty

program :: Parser Program
program = undefined

--   skipMany (symbol TokNewline)
--   decls <- many (declaration <* choice [symbol TokNewline, eof]) <?> "declarations"
--   skipMany (symbol TokNewline)
--   stmts <- many (statement <* choice [symbol TokNewline, eof]) <?> "statements"
--   skipMany (symbol TokNewline)
--   return $ Program decls stmts

-- specContent :: Parser [Stmt]
-- specContent = do
--   many statement <?> "statements"

--------------------------------------------------------------------------------

-- | Parser for SepByComma
sepBy' :: Parser (Token sep) -> Parser a -> Parser (SepBy sep a)
sepBy' delim parser = do
  x <- parser

  let f = return (Head x)
  let g = do
        sep <- delim
        xs  <- sepBy' delim parser
        return $ Delim x sep xs
  try g <|> f

sepByComma :: Parser a -> Parser (SepBy "," a)
sepByComma = sepBy' tokenComma

sepByGuardBar :: Parser a -> Parser (SepBy "|" a)
sepByGuardBar = sepBy' tokenGuardBar

-- for building parsers for tokens
adapt :: Tok -> String -> Parser (Token a)
adapt t errMsg = do
  (_, loc) <- Util.getLoc (symbol t <?> errMsg)
  case loc of
    NoLoc   -> error "NoLoc when parsing token"
    Loc l r -> return $ Token l r

tokenConst :: Parser (Token "con")
tokenConst = adapt TokCon "reserved word \"con\""

tokenVar :: Parser (Token "var")
tokenVar = adapt TokVar "reserved word \"var\""

tokenData :: Parser (Token "data")
tokenData = adapt TokData "reserved word \"data\""

tokenBraceOpen :: Parser (Token "{")
tokenBraceOpen = adapt TokBraceOpen "opening curly bracket"

tokenBraceClose :: Parser (Token "}")
tokenBraceClose = adapt TokBraceClose "closing curly bracket"

tokenBracketOpen :: Parser (Token "[")
tokenBracketOpen = adapt TokBracketOpen "opening square bracket"

tokenBracketClose :: Parser (Token "]")
tokenBracketClose = adapt TokBracketClose "closing square bracket"

tokenParenOpen :: Parser (Token "(")
tokenParenOpen = adapt TokParenOpen "opening parenthesis"

tokenParenClose :: Parser (Token ")")
tokenParenClose = adapt TokParenClose "closing parenthesis"

tokenQuantOpen :: Parser (Token "<|")
tokenQuantOpen = adapt TokQuantOpen "<|"

tokenQuantOpenU :: Parser (Token "⟨")
tokenQuantOpenU = adapt TokQuantOpenU "⟨"

tokenQuantClose :: Parser (Token "|>")
tokenQuantClose = adapt TokQuantClose "|>"

tokenQuantCloseU :: Parser (Token "⟩")
tokenQuantCloseU = adapt TokQuantCloseU "⟩"

tokenSpecOpen :: Parser (Token "[!")
tokenSpecOpen = adapt TokSpecOpen "[!"

tokenSpecClose :: Parser (Token "!]")
tokenSpecClose = adapt TokSpecClose "!]"

tokenProofOpen :: Parser (Token "{-")
tokenProofOpen = adapt TokProofOpen "{-"

tokenProofClose :: Parser (Token "-}")
tokenProofClose = adapt TokProofClose "-}"

tokenColon :: Parser (Token ":")
tokenColon = adapt TokColon "colon"

tokenComma :: Parser (Token ",")
tokenComma = adapt TokComma "comma"

tokenRange :: Parser (Token "..")
tokenRange = adapt TokRange ".."

tokenArray :: Parser (Token "array")
tokenArray = adapt TokArray "reserved word \"array\""

tokenOf :: Parser (Token "of")
tokenOf = adapt TokOf "reserved word \"of\""

tokenBnd :: Parser (Token "bnd")
tokenBnd = adapt TokBnd "reserved word \"bnd\""

tokenIf :: Parser (Token "if")
tokenIf = adapt TokIf "reserved word \"if\""

tokenFi :: Parser (Token "fi")
tokenFi = adapt TokFi "reserved word \"fi\""

tokenDo :: Parser (Token "do")
tokenDo = adapt TokDo "reserved word \"do\""

tokenOd :: Parser (Token "od")
tokenOd = adapt TokOd "reserved word \"od\""

tokenCase :: Parser (Token "case")
tokenCase = adapt TokCase "reserved word \"case\""

tokenAssign :: Parser (Token ":=")
tokenAssign = adapt TokAssign ":="

tokenEQ :: Parser (Token "=")
tokenEQ = adapt TokEQ "="

tokenGuardBar :: Parser (Token "|")
tokenGuardBar = adapt TokGuardBar "|"

tokenArrow :: Parser (Either (Token "->") (Token "→"))
tokenArrow =
  choice [Left <$> adapt TokArrow "->", Right <$> adapt TokArrowU "→"]

tokenUnderscore :: Parser (Token "_")
tokenUnderscore = adapt TokUnderscore "underscore \"_\""

-- --------------------------------------------------------------------------------

-- -- | Declarations
-- declaration :: Parser Declaration
-- declaration =
--   choice
--     [ try constDeclWithProp,
--       constDecl,
--       try varDeclWithProp,
--       varDecl,
--       letDecl
--     ]
--     <?> "declaration"

-- constDecl :: Parser Declaration
-- constDecl =
--   ConstDecl
--     <$> tokenConst
--     <*> constList
--     <*> tokenColon
--     <*> type'

-- constDeclWithProp :: Parser Declaration
-- constDeclWithProp =
--   ConstDeclWithProp
--     <$> tokenConst
--     <*> constList
--     <*> tokenColon
--     <*> type'
--     <*> tokenBraceOpen
--     <*> expression
--     <*> tokenBraceClose

-- varDecl :: Parser Declaration
-- varDecl =
--   VarDecl
--     <$> tokenVar
--     <*> variableList
--     <*> tokenColon
--     <*> type'

-- varDeclWithProp :: Parser Declaration
-- varDeclWithProp =
--   VarDeclWithProp
--     <$> tokenVar
--     <*> variableList
--     <*> tokenColon
--     <*> type'
--     <*> tokenBraceOpen
--     <*> expression
--     <*> tokenBraceClose

-- letDecl :: Parser Declaration
-- letDecl =
--   LetDecl
--     <$> tokenLet
--     <*> upper
--     <*> many lower
--     <*> tokenEQ
--     <*> predicate

-- --------------------------------------------------------------------------------

-- -- | Variables and stuff

-- -- separated by commas
-- constList :: Parser (SepBy "," Name)
-- constList = sepByComma upper <?> "a list of constants separated by commas"

-- -- separated by commas
-- variableList :: Parser (SepBy "," Name)
-- variableList = sepByComma lower <?> "a list of variables separated by commas"

-- --------------------------------------------------------------------------------

-- -- | Stmts
-- statement :: Parser Stmt
-- statement =
--   choice
--     [ try assign,
--       abort,
--       try assertWithBnd,
--       spec,
--       proof,
--       assert,
--       skip,
--       loop,
--       conditional,
--       hole
--     ]
--     <?> "statement"

-- statements :: Parser [Stmt]
-- statements = sepBy statement (symbol TokNewline)

-- statements1 :: Parser [Stmt]
-- statements1 = sepBy1 statement (symbol TokNewline)

-- skip :: Parser Stmt
-- skip = withLoc $ Skip <$ symbol TokSkip

-- abort :: Parser Stmt
-- abort = withLoc $ Abort <$ symbol TokAbort

-- assert :: Parser Stmt
-- assert =
--   Assert
--     <$> tokenBraceOpen
--     <*> expression
--     <*> tokenBraceClose

-- assertWithBnd :: Parser Stmt
-- assertWithBnd = do
--   LoopInvariant
--     <$> tokenBraceOpen
--     <*> predicate
--     <*> tokenComma
--     <*> tokenBnd
--     <*> tokenColon
--     <*> expression
--     <*> tokenBraceClose

-- assign :: Parser Stmt
-- assign =
--   Assign
--     <$> sepByComma lower
--     <*> tokenAssign
--     <*> sepByComma expression

-- loop :: Parser Stmt
-- loop =
--   block'
--     Do
--     tokenDo
--     (sepByGuardBar guardedCommand)
--     tokenOd

-- conditional :: Parser Stmt
-- conditional =
--   block'
--     If
--     tokenIf
--     (sepByGuardBar guardedCommand)
--     tokenFi

-- guardedCommands :: Parser [GdCmd]
-- guardedCommands = sepBy1 guardedCommand $ do
--   symbol TokGuardBar <?> "|"

-- guardedCommand :: Parser GdCmd
-- guardedCommand =
--   GdCmd
--     <$> predicate
--     <*> ((Left <$> tokenArrow) <|> (Right <$> tokenArrowU))
--     <*> block statements1

-- hole :: Parser Stmt
-- hole = withLoc $ SpecQM <$ (symbol TokQM <?> "?")

-- spec :: Parser Stmt
-- spec =
--   Spec
--     <$> tokenSpecOpen
--     <* specContent
--     <* takeWhileP (Just "anything other than '!}'") isTokSpecClose
--     <*> tokenSpecClose
--   where
--     isTokSpecClose :: L Tok -> Bool
--     isTokSpecClose (L _ TokSpecClose) = False
--     isTokSpecClose _ = True

proofAnchors :: Parser Stmt
proofAnchors =
  Proof <$> tokenProofOpen <*> many proofAnchor <*> tokenProofClose
 where
  proofAnchor :: Parser ProofAnchor
  proofAnchor = do
    (hash, range) <- getRange $ extract extractHash
    skipProof
    return $ ProofAnchor hash range
  
  skipProof :: Parser ()
  skipProof = void $ takeWhileP (Just "anything other than '-]' or another proof anchor")
                       isTokProofCloseOrProofAnchor

  isTokProofCloseOrProofAnchor :: L Tok -> Bool
  isTokProofCloseOrProofAnchor (L _ TokProofClose     ) = False
  isTokProofCloseOrProofAnchor (L _ (TokProofAnchor _)) = False
  isTokProofCloseOrProofAnchor _                        = True

  extractHash (TokProofAnchor s) = Just (Text.pack s)
  extractHash _                  = Nothing



  -- pProofAnchorsOrProofEnd = do
  --   let anchorOrEnd =
  --         choice [lexProofEndF >> return False, pProofAnchorF >> return True]
  --   (_, continue) <- manyTill_ anySingle (lookAhead anchorOrEnd)
  --   if continue
  --     then do
  --       x  <- pProofAnchorF
  --       xs <- pProofAnchorsOrProofEnd
  --       return (x : xs)
  --     else return []


-- --------------------------------------------------------------------------------
------------------------------------------
-- Expressions 
------------------------------------------

expressions :: Parser [Expr]
expressions =
  sepBy1 expression (symbol TokComma)
    <?> "a list of expressions separated by commas"

predicate :: Parser Expr
predicate = expression <?> "predicate"

expression :: Parser Expr
expression = makeExprParser (term <|> caseOf) chainOpTable <?> "expression"
 where
  chainOpTable :: [[Operator Parser Expr]]
  chainOpTable =
    [ -- =
      [InfixL $ binary (ChainOp . EQ) TokEQ]
      -- ~, <, <=, >, >=
    , [ InfixL $ binary (ChainOp . NEQ) TokNEQ
      , InfixL $ binary (ChainOp . NEQU) TokNEQU
      , InfixL $ binary (ChainOp . LT) TokLT
      , InfixL $ binary (ChainOp . LTE) TokLTE
      , InfixL $ binary (ChainOp . LTEU) TokLTEU
      , InfixL $ binary (ChainOp . GT) TokGT
      , InfixL $ binary (ChainOp . GTE) TokGTE
      , InfixL $ binary (ChainOp . GTEU) TokGTEU
      ]
      -- &&
    , [ InfixL $ binary (ArithOp . Conj) TokConj
      , InfixL $ binary (ArithOp . ConjU) TokConjU
      ]
      --- ||
    , [ InfixL $ binary (ArithOp . Disj) TokDisj
      , InfixL $ binary (ArithOp . DisjU) TokDisjU
      ]
      -- =>
    , [ InfixL $ binary (ArithOp . Implies) TokImpl
      , InfixL $ binary (ArithOp . ImpliesU) TokImplU
      ]
      -- <=>
    , [ InfixL $ binary (ChainOp . EQProp) TokEQProp
      , InfixL $ binary (ChainOp . EQPropU) TokEQPropU
      ]
    ]

  application :: Parser (Expr -> Expr)
  application = do
    terms <- many term
    return $ \func -> do
      let app inner t = App inner t
      foldl app func terms

  unary :: (Loc -> Op) -> Tok -> Parser (Expr -> Expr)
  unary operator' tok = do
    (op, loc) <- Util.getLoc (operator' <$ symbol tok)
    return $ \result -> App (Expr.Op (op loc)) result

  binary :: (Loc -> Op) -> Tok -> Parser (Expr -> Expr -> Expr)
  binary operator' tok = do
    (op, loc) <- Util.getLoc (operator' <$ symbol tok)
    return $ \x y -> App (App (Expr.Op (op loc)) x) y

  parensExpr :: Parser Expr
  parensExpr = Paren <$> tokenParenOpen <*> expression <*> tokenParenClose

  caseOf :: Parser Expr
  caseOf =
    Case <$> tokenCase <*> expression <*> tokenOf <*> block (many caseClause)

  caseClause :: Parser CaseClause
  caseClause = CaseClause <$> pattern' <*> tokenArrow <*> expression

  term :: Parser Expr
  term = makeExprParser term' arithTable
   where
    arithTable :: [[Operator Parser Expr]]
    arithTable =
      [ [Postfix application] -- NOTE: InfixL ?
      , [Prefix $ unary (ArithOp . NegNum) TokSub]
      , [InfixN $ binary (ArithOp . Exp) TokExp]
      , [ InfixN $ binary (ArithOp . Max) TokMax
        , InfixN $ binary (ArithOp . Min) TokMin
        ]
      , [InfixL $ binary (ArithOp . Mod) TokMod]
      , [ InfixL $ binary (ArithOp . Mul) TokMul
        , InfixL $ binary (ArithOp . Div) TokDiv
        ]
      , [ InfixL $ binary (ArithOp . Add) TokAdd
        , InfixL $ binary (ArithOp . Sub) TokSub
        ]
      , [ Prefix $ unary (ArithOp . Neg) TokNeg
        , Prefix $ unary (ArithOp . NegU) TokNegU
        ]
      ]

    term' :: Parser Expr
    term' =
      choice
          [ Lit <$> literal
          , try array
          , parensExpr
          , Var <$> lower
          , Const <$> upper
          , Quant
          <$> choice [Left <$> tokenQuantOpen, Right <$> tokenQuantOpenU]
          <*> choice [Left <$> operator, Right <$> term']
          <*> some lower
          <*> tokenColon
          <*> expression
          <*> tokenColon
          <*> expression
          <*> choice [Left <$> tokenQuantClose, Right <$> tokenQuantCloseU]
          ]
        <?> "term"

    -- shoule parse A[A[i]], A[i1][i2]...[in]
    array :: Parser Expr
    array = do
      arr     <- choice [parensExpr, Var <$> lower, Const <$> upper]
      indices <- some $ do
        open  <- tokenBracketOpen
        xs    <- term
        close <- tokenBracketClose
        return (open, xs, close)
      return $ helper arr indices
     where
      helper :: Expr -> [(Token "[", Expr, Token "]")] -> Expr
      helper a []               = a
      helper a ((o, x, c) : xs) = helper (Arr a o x c) xs

  operator :: Parser Op
  operator = choice [ChainOp <$> chainOp, ArithOp <$> arithOp] <?> "operator"
   where
    chainOp :: Parser ChainOp
    chainOp = choice []

    arithOp :: Parser ArithOp
    arithOp = choice []

-- TODO: LitChar 
literal :: Parser Lit
literal =
  withRange
      (choice
        [ LitBool True <$ symbol TokTrue
        , LitBool False <$ symbol TokFalse
        , LitInt <$> integer
        ]
      )
    <?> "literal"

------------------------------------------
-- Pattern matching
------------------------------------------

pattern' :: Parser Pattern
pattern' = choice
  [ PattLit <$> literal
  , PattParen <$> tokenParenOpen <*> pattern' <*> tokenParenClose
  , PattWildcard <$> tokenUnderscore
  , PattBinder <$> lower
  , PattConstructor <$> upper <*> many pattern'
  ]

--------------------------------------------------------------------------------

-- | Type
type' :: Parser Type
type' = ignoreIndentations $ do
  makeExprParser term table <?> "type"
 where
  table :: [[Operator Parser Type]]
  table = [[InfixR function]]

  function :: Parser (Type -> Type -> Type)
  function = ignoreIndentations $ do
    arrow <- tokenArrow
    return $ \x y -> TFunc x arrow y

  term :: Parser Type
  term = ignoreIndentations $ do
    parensType <|> array <|> try typeVar <|> typeName <?> "type term"

  parensType :: Parser Type
  parensType = TParen <$> tokenParenOpen <*> type' <*> tokenParenClose


  typeVar :: Parser Type
  typeVar = TVar <$> lower

  typeName :: Parser Type
  typeName = TCon <$> upper <*> many lower
  --  where
    -- isBaseType :: Tok -> Maybe (Range -> TBase)
    -- isBaseType (TokUpperName "Int" ) = Just TInt
    -- isBaseType (TokUpperName "Bool") = Just TBool
    -- isBaseType (TokUpperName "Char") = Just TChar
    -- isBaseType _                     = Nothing

  array :: Parser Type
  array = TArray <$> tokenArray <*> interval <*> tokenOf <*> type'

  interval :: Parser Interval
  interval = Interval <$> endpointOpening <*> tokenRange <*> endpointClosing

  endpointOpening :: Parser EndpointOpen
  endpointOpening = choice
    [ IncludingOpening <$> tokenBracketOpen <*> expression
    , ExcludingOpening <$> tokenParenOpen <*> expression
    ]

  endpointClosing :: Parser EndpointClose
  endpointClosing = do
    expr <- expression
    choice
      [ IncludingClosing expr <$> tokenBracketClose
      , ExcludingClosing expr <$> tokenParenClose
      ]

--------------------------------------------------------------------------------

-- | Combinators
block :: Parser a -> Parser a
block parser = do
  Util.ignore TokIndent <?> "indentation"
  result <- parser
  Util.ignore TokDedent <?> "dedentation"
  return result

block' :: (l -> x -> r -> y) -> Parser l -> Parser x -> Parser r -> Parser y
block' constructor open parser close = do
  a <- open
  symbol TokIndent <?> "indentation"
  b <- parser
  c <- choice
    [ do
          -- the ideal case
      symbol TokDedent <?> "dedentation"
      close
    , do
          -- the fucked up case:
          --  the tokener is not capable of handling cases like "if True -> skip fi"
          --  because it's not possible to determine the number of `TokDedent` before `TokFi`
      c <- close
      symbol TokDedent <?> "dedentation"
      return c
    ]
  return $ constructor a b c

-- consumes 0 or more newlines/indents/dedents afterwards
ignoreIndentations :: Parser a -> Parser a
ignoreIndentations parser = do
  result <- parser
  void $ many (Util.ignoreP indentationRelated)
  return result
 where
  indentationRelated TokIndent = True
  indentationRelated TokDedent = True
  indentationRelated _         = False

-- consumes 1 or more newlines
expectNewline :: Parser ()
expectNewline = do
  -- see if the latest accepcted token is TokNewline
  t <- lift Util.getLastToken
  case t of
    Just TokNewline -> return ()
    _               -> void $ some (Util.ignore TokNewline)

symbol :: Tok -> Parser ()
symbol = Util.symbol

parens :: Parser a -> Parser (a, Range)
parens parser = do
  (_, start) <- getRange (symbol TokParenOpen <?> "opening parenthesis")
  result     <- parser
  (_, end)   <- getRange (symbol TokParenClose <?> "closing parenthesis")
  return (result, start <> end)

braces :: Parser a -> Parser (a, Range)
braces parser = do
  (_, start) <- getRange (symbol TokBraceOpen <?> "opening braces")
  result     <- parser
  (_, end)   <- getRange (symbol TokBraceClose <?> "closing braces")
  return (result, start <> end)

upperName :: Parser Text
upperName = extract p
 where
  p (TokUpperName s) = Just s
  p _                = Nothing

upper :: Parser Name
upper =
  withLoc (Name <$> upperName)
    <?> "identifier that starts with a uppercase letter"

lowerName :: Parser Text
lowerName = extract p
 where
  p (TokLowerName s) = Just s
  p _                = Nothing

lower :: Parser Name
lower =
  withLoc (Name <$> lowerName)
    <?> "identifier that starts with a lowercase letter"

identifier :: Parser Name
identifier =
  withLoc (choice [Name <$> lowerName, Name <$> upperName]) <?> "identifier"

integer :: Parser Int
integer = extract p <?> "integer"
 where
  p (TokInt s) = Just s
  p _          = Nothing
