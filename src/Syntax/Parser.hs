{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}

module Syntax.Parser where

import Control.Monad (void)
import Control.Monad.Combinators.Expr
import Control.Monad.State (lift)
import Data.Loc
import Data.Text.Lazy (Text)
import Data.Void
import Syntax.Concrete
import Syntax.Parser.Lexer
import Syntax.Parser.Util
  ( PosLog,
    extract,
  )
import qualified Syntax.Parser.Util as Util
import Text.Megaparsec hiding
  ( ParseError,
    Pos,
    State,
    Token,
    parse,
  )
import qualified Text.Megaparsec as Mega
import Prelude hiding (Ordering (..))

--------------------------------------------------------------------------------

-- | States for source location bookkeeping
type Parser = ParsecT Void TokStream (PosLog Tok)

type SyntacticError = (Loc, String)

parse :: Parser a -> FilePath -> TokStream -> Either [SyntacticError] a
parse parser filepath tokenStream =
  case Util.runPosLog (runParserT parser filepath tokenStream) of
    Left e -> Left (fromParseErrorBundle e)
    Right x -> Right x
  where
    fromParseErrorBundle ::
      ShowErrorComponent e => ParseErrorBundle TokStream e -> [SyntacticError]
    fromParseErrorBundle (ParseErrorBundle errors posState) =
      snd $
        foldr f (posState, []) errors
      where
        f ::
          ShowErrorComponent e =>
          Mega.ParseError TokStream e ->
          (PosState TokStream, [SyntacticError]) ->
          (PosState TokStream, [SyntacticError])
        f err (initial, accum) =
          let (_, next) = reachOffset (errorOffset err) initial
           in (next, (getLoc err, parseErrorTextPretty err) : accum)

        getLoc :: ShowErrorComponent e => Mega.ParseError TokStream e -> Loc
        -- get the Loc of all unexpected tokens
        getLoc (TrivialError _ (Just (Tokens xs)) _) = foldMap locOf xs
        getLoc _ = mempty

program :: Parser Program
program = do
  skipMany (symbol TokNewline)
  decls <- many (declaration <* choice [symbol TokNewline, eof]) <?> "declarations"
  skipMany (symbol TokNewline)
  stmts <- many (statement <* choice [symbol TokNewline, eof]) <?> "statements"
  skipMany (symbol TokNewline)
  return $ Program decls stmts

specContent :: Parser [Stmt]
specContent = do
  many statement <?> "statements"

--------------------------------------------------------------------------------

-- | Parser for SepByComma
sepBy' :: Parser (Token sep) -> Parser a -> Parser (SepBy sep a)
sepBy' delim parser = do
  x <- parser

  let f = return (Head x)
  let g = do
        sep <- delim
        xs <- sepBy' delim parser
        return $ Delim x sep xs
  try g <|> f

sepByComma :: Parser a -> Parser (SepBy 'TokComma a)
sepByComma = sepBy' tokenComma

sepByGuardBar :: Parser a -> Parser (SepBy 'TokGuardBar a)
sepByGuardBar = sepBy' tokenGuardBar

-- for building parsers for tokens
adapt :: Tok -> String -> Parser (Token a)
adapt t errMsg = do
  (_, loc) <- Util.getLoc (symbol t <?> errMsg)
  case loc of
    NoLoc -> error "NoLoc when parsing token"
    Loc l r -> return $ Token l r

tokenConst :: Parser (Token 'TokCon)
tokenConst = adapt TokCon "reserved word \"con\""

tokenVar :: Parser (Token 'TokVar)
tokenVar = adapt TokVar "reserved word \"var\""

tokenLet :: Parser (Token 'TokLet)
tokenLet = adapt TokLet "reserved word \"let\""

tokenBraceOpen :: Parser (Token 'TokBraceOpen)
tokenBraceOpen = adapt TokBraceOpen "opening curly bracket"

tokenBraceClose :: Parser (Token 'TokBraceClose)
tokenBraceClose = adapt TokBraceClose "closing curly bracket"

tokenBracketOpen :: Parser (Token 'TokBracketOpen)
tokenBracketOpen = adapt TokBracketOpen "opening square bracket"

tokenBracketClose :: Parser (Token 'TokBracketClose)
tokenBracketClose = adapt TokBracketClose "closing square bracket"

tokenParenOpen :: Parser (Token 'TokParenOpen)
tokenParenOpen = adapt TokParenOpen "opening parenthesis"

tokenParenClose :: Parser (Token 'TokParenClose)
tokenParenClose = adapt TokParenClose "closing parenthesis"

tokenQuantOpen :: Parser (Token 'TokQuantOpen)
tokenQuantOpen = adapt TokQuantOpen "<|"

tokenQuantOpenU :: Parser (Token 'TokQuantOpenU)
tokenQuantOpenU = adapt TokQuantOpenU "⟨"

tokenQuantClose :: Parser (Token 'TokQuantClose)
tokenQuantClose = adapt TokQuantClose "|>"

tokenQuantCloseU :: Parser (Token 'TokQuantCloseU)
tokenQuantCloseU = adapt TokQuantCloseU "⟩"

tokenSpecOpen :: Parser (Token 'TokSpecOpen)
tokenSpecOpen = adapt TokSpecOpen "{!"

tokenSpecClose :: Parser (Token 'TokSpecClose)
tokenSpecClose = adapt TokSpecClose "!}"

tokenProofOpen :: Parser (Token 'TokProofOpen)
tokenProofOpen = adapt TokProofOpen "{-"

tokenProofClose :: Parser (Token 'TokProofClose)
tokenProofClose = adapt TokProofClose "-}"

tokenColon :: Parser (Token 'TokColon)
tokenColon = adapt TokColon "colon"

tokenComma :: Parser (Token 'TokComma)
tokenComma = adapt TokComma "comma"

tokenRange :: Parser (Token 'TokRange)
tokenRange = adapt TokRange ".."

tokenArray :: Parser (Token 'TokArray)
tokenArray = adapt TokArray "reserved word \"array\""

tokenOf :: Parser (Token 'TokOf)
tokenOf = adapt TokOf "reserved word \"of\""

tokenBnd :: Parser (Token 'TokBnd)
tokenBnd = adapt TokBnd "reserved word \"bnd\""

tokenIf :: Parser (Token 'TokIf)
tokenIf = adapt TokIf "reserved word \"if\""

tokenFi :: Parser (Token 'TokFi)
tokenFi = adapt TokFi "reserved word \"fi\""

tokenDo :: Parser (Token 'TokDo)
tokenDo = adapt TokDo "reserved word \"do\""

tokenOd :: Parser (Token 'TokOd)
tokenOd = adapt TokOd "reserved word \"od\""

tokenAssign :: Parser (Token 'TokAssign)
tokenAssign = adapt TokAssign ":="

tokenEQ :: Parser (Token 'TokEQ)
tokenEQ = adapt TokEQ "="

tokenGuardBar :: Parser (Token 'TokGuardBar)
tokenGuardBar = adapt TokGuardBar "|"

tokenArrow :: Parser (Token 'TokArrow)
tokenArrow = adapt TokArrow "->"

tokenArrowU :: Parser (Token 'TokArrowU)
tokenArrowU = adapt TokArrowU "→"

--------------------------------------------------------------------------------

-- | Declarations
declaration :: Parser Declaration
declaration =
  choice
    [ try constDeclWithProp,
      constDecl,
      try varDeclWithProp,
      varDecl,
      letDecl
    ]
    <?> "declaration"

constDecl :: Parser Declaration
constDecl =
  ConstDecl
    <$> tokenConst
    <*> constList
    <*> tokenColon
    <*> type'

constDeclWithProp :: Parser Declaration
constDeclWithProp =
  ConstDeclWithProp
    <$> tokenConst
    <*> constList
    <*> tokenColon
    <*> type'
    <*> tokenBraceOpen
    <*> expression
    <*> tokenBraceClose

varDecl :: Parser Declaration
varDecl =
  VarDecl
    <$> tokenVar
    <*> variableList
    <*> tokenColon
    <*> type'

varDeclWithProp :: Parser Declaration
varDeclWithProp =
  VarDeclWithProp
    <$> tokenVar
    <*> variableList
    <*> tokenColon
    <*> type'
    <*> tokenBraceOpen
    <*> expression
    <*> tokenBraceClose

letDecl :: Parser Declaration
letDecl =
  LetDecl
    <$> tokenLet
    <*> upper
    <*> many lower
    <*> tokenEQ
    <*> predicate

--------------------------------------------------------------------------------

-- | Variables and stuff

-- separated by commas
constList :: Parser (SepBy 'TokComma Name)
constList = sepByComma upper <?> "a list of constants separated by commas"

-- separated by commas
variableList :: Parser (SepBy 'TokComma Name)
variableList = sepByComma lower <?> "a list of variables separated by commas"

--------------------------------------------------------------------------------

-- | Stmts
statement :: Parser Stmt
statement =
  choice
    [ try assign,
      abort,
      try assertWithBnd,
      spec,
      proof,
      assert,
      skip,
      loop,
      conditional,
      hole
    ]
    <?> "statement"

statements :: Parser [Stmt]
statements = sepBy statement (symbol TokNewline)

statements1 :: Parser [Stmt]
statements1 = sepBy1 statement (symbol TokNewline)

skip :: Parser Stmt
skip = withLoc $ Skip <$ symbol TokSkip

abort :: Parser Stmt
abort = withLoc $ Abort <$ symbol TokAbort

assert :: Parser Stmt
assert =
  Assert
    <$> tokenBraceOpen
    <*> expression
    <*> tokenBraceClose

assertWithBnd :: Parser Stmt
assertWithBnd = do
  LoopInvariant
    <$> tokenBraceOpen
    <*> predicate
    <*> tokenComma
    <*> tokenBnd
    <*> tokenColon
    <*> expression
    <*> tokenBraceClose

assign :: Parser Stmt
assign =
  Assign
    <$> sepByComma lower
    <*> tokenAssign
    <*> sepByComma expression

loop :: Parser Stmt
loop =
  block'
    Do
    tokenDo
    (sepByGuardBar guardedCommand)
    tokenOd

conditional :: Parser Stmt
conditional =
  block'
    If
    tokenIf
    (sepByGuardBar guardedCommand)
    tokenFi

guardedCommands :: Parser [GdCmd]
guardedCommands = sepBy1 guardedCommand $ do
  symbol TokGuardBar <?> "|"

guardedCommand :: Parser GdCmd
guardedCommand =
  GdCmd
    <$> predicate
    <*> ((Left <$> tokenArrow) <|> (Right <$> tokenArrowU))
    <*> block statements1

hole :: Parser Stmt
hole = withLoc $ SpecQM <$ (symbol TokQM <?> "?")

spec :: Parser Stmt
spec =
  Spec
    <$> tokenSpecOpen
    <* specContent
    <* takeWhileP (Just "anything other than '!}'") isTokSpecClose
    <*> tokenSpecClose
  where
    isTokSpecClose :: L Tok -> Bool
    isTokSpecClose (L _ TokSpecClose) = False
    isTokSpecClose _ = True

proof :: Parser Stmt
proof =
  Proof
    <$> tokenProofOpen
    <* specContent
    <* takeWhileP (Just "anything other than '-}'") isTokProofClose
      <*> tokenProofClose
  where
    isTokProofClose :: L Tok -> Bool
    isTokProofClose (L _ TokProofClose) = False
    isTokProofClose _ = True

--------------------------------------------------------------------------------

-- | Expressions
expressionList :: Parser [Expr]
expressionList =
  sepBy1 expression (symbol TokComma)
    <?> "a list of expressions separated by commas"

predicate :: Parser Expr
predicate = expression <?> "predicate"

expression :: Parser Expr
expression = makeExprParser term table <?> "expression"
  where
    table :: [[Operator Parser Expr]]
    table =
      [ [Postfix application],
        [InfixL $ binary Mod TokMod],
        [ InfixL $ binary Mul TokMul,
          InfixL $ binary Div TokDiv
        ],
        [ InfixL $ binary Add TokAdd,
          InfixL $ binary Sub TokSub
        ],
        [ InfixL $ binary NEQ TokNEQ,
          InfixL $ binary NEQU TokNEQU,
          InfixL $ binary LT TokLT,
          InfixL $ binary LTE TokLTE,
          InfixL $ binary LTEU TokLTEU,
          InfixL $ binary GT TokGT,
          InfixL $ binary GTE TokGTE,
          InfixL $ binary GTEU TokGTEU
        ],
        [InfixL $ binary EQ TokEQ],
        [Prefix $ unary Neg TokNeg, Prefix $ unary NegU TokNegU],
        [InfixL $ binary Conj TokConj, InfixL $ binary ConjU TokConjU],
        [InfixL $ binary Disj TokDisj, InfixL $ binary DisjU TokDisjU],
        [InfixR $ binary Implies TokImpl, InfixR $ binary ImpliesU TokImplU]
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
      return $ \result -> App (Op (op loc)) result

    binary :: (Loc -> Op) -> Tok -> Parser (Expr -> Expr -> Expr)
    binary operator' tok = do
      (op, loc) <- Util.getLoc (operator' <$ symbol tok)
      return $ \x y -> App (App (Op (op loc)) x) y

    parensExpr :: Parser Expr
    parensExpr =
      Paren
        <$> tokenParenOpen
          <*> expression
          <*> tokenParenClose

    term :: Parser Expr
    term = try term' <|> parensExpr
      where
        term' :: Parser Expr
        term' =
          choice
            [ Var <$> lower,
              Const <$> upper,
              Lit <$> literal,
              -- Op <$ symbol TokParenOpen <*> operator <* symbol TokParenClose,
              Quant
                <$> choice [Left <$> tokenQuantOpen, Right <$> tokenQuantOpenU]
                <*> operator
                <*> some lower
                <*> tokenColon
                <*> expression
                <*> tokenColon
                <*> expression
                <*> choice [Left <$> tokenQuantClose, Right <$> tokenQuantCloseU]
            ]
            <?> "term"

    -- quantOp :: Parser Op
    -- quantOp = operator <|> do
    --                           (_, _start) <- Util.getLoc (symbol TokParenOpen <?> "opening parenthesis")
    --                           op <- operator
    --                           (_, _end) <- Util.getLoc (symbol TokParenClose <?> "closing parenthesis")
    --                           return op
    -- choice
    --   [ do
    --       -- (_, start) <- Util.getLoc (symbol TokParenOpen <?> "opening parenthesis")
    --       -- (op, loc) <- Util.getLoc operator
    --       -- (_, end) <- Util.getLoc (symbol TokParenClose <?> "closing parenthesis")
    --       return $ op
    --     -- do
    --     --   op <- term
    --     --   return $ case op of
    --     --     Op (Add l) loc -> Op (Sum l) loc
    --     --     Op (Conj l) loc -> Op (Forall l) loc
    --     --     Op (Disj l) loc -> Op (Exists l) loc
    --     --     others -> others,
    --     -- parensExpr
    --   ]

    -- -- replace "+", "∧", and "∨" in Quant with "Σ", "∀", and "∃"
    -- quantOp :: Parser Expr
    -- quantOp = do
    --   op <- term
    --   return $ case op of
    --     Op Add loc -> Op Sum loc
    --     Op Conj loc -> Op Forall loc
    --     Op Disj loc -> Op Exists loc
    --     others -> others

    literal :: Parser Lit
    literal =
      withLoc
        ( choice
            [ LitBool True <$ symbol TokTrue,
              LitBool False <$ symbol TokFalse,
              LitInt <$> integer
            ]
        )
        <?> "literal"

    operator :: Parser Op
    operator =
      withLoc
        ( choice
            [ EQ <$ symbol TokEQ,
              NEQ <$ symbol TokNEQ,
              NEQU <$ symbol TokNEQU,
              LTE <$ symbol TokLTE,
              LTEU <$ symbol TokLTEU,
              GTE <$ symbol TokGTE,
              GTEU <$ symbol TokGTEU,
              LT <$ symbol TokLT,
              GT <$ symbol TokGT,
              Implies <$ symbol TokImpl,
              ImpliesU <$ symbol TokImplU,
              Conj <$ symbol TokConj,
              ConjU <$ symbol TokConjU,
              Disj <$ symbol TokDisj,
              DisjU <$ symbol TokDisjU,
              Neg <$ symbol TokNeg,
              NegU <$ symbol TokNegU,
              Add <$ symbol TokAdd,
              Sub <$ symbol TokSub,
              Mul <$ symbol TokMul,
              Div <$ symbol TokDiv,
              Mod <$ symbol TokMod
            ]
        )
        <?> "operator"

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
      arrow <- choice [Left <$> tokenArrow, Right <$> tokenArrowU]
      return $ \x y -> TFunc x arrow y

    term :: Parser Type
    term = ignoreIndentations $ do
      parensType <|> array <|> base <?> "type term"

    parensType :: Parser Type
    parensType = TParen <$> tokenParenOpen <*> type' <*> tokenParenClose

    base :: Parser Type
    base = TBase <$> withLoc (extract isBaseType)
      where
        isBaseType :: Tok -> Maybe (Loc -> TBase)
        isBaseType (TokUpperName "Int") = Just TInt
        isBaseType (TokUpperName "Bool") = Just TBool
        isBaseType (TokUpperName "Char") = Just TChar
        isBaseType _ = Nothing

    array :: Parser Type
    array = TArray <$> tokenArray <*> interval <*> tokenOf <*> type'

    endpointOpening :: Parser EndpointOpen
    endpointOpening =
      choice
        [ IncludingOpening <$> tokenBracketOpen <*> expression,
          ExcludingOpening <$> tokenParenOpen <*> expression
        ]

    endpointClosing :: Parser EndpointClose
    endpointClosing = do
      expr <- expression
      choice
        [ IncludingClosing expr <$> tokenBracketClose,
          ExcludingClosing expr <$> tokenParenClose
        ]

    interval :: Parser Interval
    interval =
      Interval
        <$> endpointOpening
        <*> tokenRange
        <*> endpointClosing

-- withLoc $ do
-- start <-
--   choice
--     [Excluding <$ symbol TokParenOpen, Including <$ symbol TokBracketOpen]
-- i <- expression
-- symbol TokRange
-- j <- expression
-- end <-
--   choice
--     [Excluding <$ symbol TokParenClose, Including <$ symbol TokBracketClose]
-- return $ Interval (start i) (end j)

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
  c <-
    choice
      [ do
          -- the ideal case
          symbol TokDedent <?> "dedentation"
          close,
        do
          -- the fucked up case:
          --  the lexer is not capable of handling cases like "if True -> skip fi"
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
    indentationRelated _ = False

-- consumes 1 or more newlines
expectNewline :: Parser ()
expectNewline = do
  -- see if the latest accepcted token is TokNewline
  t <- lift Util.getLastToken
  case t of
    Just TokNewline -> return ()
    _ -> void $ some (Util.ignore TokNewline)

symbol :: Tok -> Parser ()
symbol = Util.symbol

withLoc :: Parser (Loc -> a) -> Parser a
withLoc = Util.withLoc

parens :: Parser a -> Parser (a, Loc)
parens parser = do
  (_, start) <- Util.getLoc (symbol TokParenOpen <?> "opening parenthesis")
  result <- parser
  (_, end) <- Util.getLoc (symbol TokParenClose <?> "closing parenthesis")
  let loc = start <--> end
  return (result, loc)

braces :: Parser a -> Parser (a, Loc)
braces parser = do
  (_, start) <- Util.getLoc (symbol TokBraceOpen <?> "opening braces")
  result <- parser
  (_, end) <- Util.getLoc (symbol TokBraceClose <?> "closing braces")
  let loc = start <--> end
  return (result, loc)

upperName :: Parser Text
upperName = extract p
  where
    p (TokUpperName s) = Just s
    p _ = Nothing

upper :: Parser Name
upper =
  withLoc (Name <$> upperName)
    <?> "identifier that starts with a uppercase letter"

lowerName :: Parser Text
lowerName = extract p
  where
    p (TokLowerName s) = Just s
    p _ = Nothing

lower :: Parser Name
lower =
  withLoc (Name <$> lowerName)
    <?> "identifier that starts with a lowercase letter"

integer :: Parser Int
integer = extract p <?> "integer"
  where
    p (TokInt s) = Just s
    p _ = Nothing
