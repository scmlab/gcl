{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}

module Syntax.Parser where

import Control.Monad (void)
import Control.Monad.Combinators.Expr
import Control.Monad.State (lift)
import Data.Loc
import Data.Text.Lazy (Text)
import Data.Void
import Syntax.Concrete2 hiding
  ( binary,
    unary,
  )
-- import Syntax.ConstExpr2
import Syntax.Location ()
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
  withLoc $ do
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
        Token start _end <- delim
        xs <- sepBy' delim parser
        return $ Delim x start xs
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

tokenBraceStart :: Parser (Token 'TokBraceStart)
tokenBraceStart = adapt TokBraceStart "opening curly bracket"

tokenBraceEnd :: Parser (Token 'TokBraceEnd)
tokenBraceEnd = adapt TokBraceEnd "closing curly bracket"

tokenParenStart :: Parser (Token 'TokParenStart)
tokenParenStart = adapt TokParenStart "opening parenthesis"

tokenParenEnd :: Parser (Token 'TokParenEnd)
tokenParenEnd = adapt TokParenEnd "closing parenthesis"

tokenColon :: Parser (Token 'TokColon)
tokenColon = adapt TokColon "colon"

tokenComma :: Parser (Token 'TokComma)
tokenComma = adapt TokComma "comma"

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
    <*> tokenBraceStart
    <*> expression
    <*> tokenBraceEnd

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
    <*> tokenBraceStart
    <*> expression
    <*> tokenBraceEnd

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
      repetition,
      selection,
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
    <$> tokenBraceStart
    <*> expression
    <*> tokenBraceEnd

assertWithBnd :: Parser Stmt
assertWithBnd = do
  LoopInvariant
    <$> tokenBraceStart
    <*> predicate
    <*> tokenComma
    <*> tokenBnd
    <*> tokenColon
    <*> expression
    <*> tokenBraceEnd

assign :: Parser Stmt
assign =
  Assign
    <$> sepByComma lower
    <*> tokenAssign
    <*> sepByComma expression

repetition :: Parser Stmt
repetition =
  Do
    <$> tokenDo
    <*> sepByGuardBar guardedCommand
    <*> tokenOd

selection :: Parser Stmt
selection =
  withLoc $
    If <$> do
      block'
        (symbol TokIf <?> "if")
        guardedCommands
        (symbol TokFi <?> "fi")

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
spec = withLoc $ do
  symbol TokSpecStart <?> "{!"
  -- expectNewline <?> "<newline> after a the start of a Spec"
  _ <- specContent
  _ <- takeWhileP (Just "anything other than '!}'") isTokSpecEnd
  symbol TokSpecEnd <?> "!}"
  -- expectNewline <?> "<newline> after a the end of a Spec"

  return Spec
  where
    isTokSpecEnd :: L Tok -> Bool
    isTokSpecEnd (L _ TokSpecEnd) = False
    isTokSpecEnd _ = True

proof :: Parser Stmt
proof = withLoc $ do
  symbol TokProofStart <?> "{-"
  -- expectNewline <?> "<newline> after a the start of a Proof"
  _ <- specContent
  _ <- takeWhileP (Just "anything other than '-}'") isTokProofEnd
  symbol TokProofEnd <?> "-}"
  -- expectNewline <?> "<newline> after a the end of a Proof"

  return Spec
  where
    isTokProofEnd :: L Tok -> Bool
    isTokProofEnd (L _ TokProofEnd) = False
    isTokProofEnd _ = True

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
        let app inner t = App inner t (func <--> t)
        foldl app func terms

    unary :: (Loc -> Op) -> Tok -> Parser (Expr -> Expr)
    unary operator' tok = do
      (op, loc) <- Util.getLoc (operator' <$ symbol tok)
      return $ \result -> App (Op (op loc) loc) result (loc <--> result)

    binary :: (Loc -> Op) -> Tok -> Parser (Expr -> Expr -> Expr)
    binary operator' tok = do
      (op, loc) <- Util.getLoc (operator' <$ symbol tok)
      return $ \x y -> App (App (Op (op loc) loc) x (x <--> loc)) y (x <--> y)

    parensExpr :: Parser Expr
    parensExpr =
      Paren
        <$> tokenParenStart
          <*> expression
          <*> tokenParenEnd

    term :: Parser Expr
    term = try term' <|> parensExpr
      where
        term' :: Parser Expr
        term' =
          withLoc
            ( choice
                [ Var <$> lower,
                  Const <$> upper,
                  Lit <$> literal,
                  -- Op <$ symbol TokParenStart <*> operator <* symbol TokParenEnd,
                  Quant
                    <$> quantStart
                    <*> operator
                    <*> some lower
                    <*> withLoc (id <$ symbol TokColon)
                    <*> expression
                    <*> withLoc (id <$ symbol TokColon)
                    <*> expression
                    <*> quantEnd
                ]
            )
            <?> "term"

        -- quantOp :: Parser Op
        -- quantOp = operator <|> do
        --                           (_, _start) <- Util.getLoc (symbol TokParenStart <?> "opening parenthesis")
        --                           op <- operator
        --                           (_, _end) <- Util.getLoc (symbol TokParenEnd <?> "closing parenthesis")
        --                           return op
        -- choice
        --   [ do
        --       -- (_, start) <- Util.getLoc (symbol TokParenStart <?> "opening parenthesis")
        --       -- (op, loc) <- Util.getLoc operator
        --       -- (_, end) <- Util.getLoc (symbol TokParenEnd <?> "closing parenthesis")
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

        quantStart :: Parser (Bool, Loc)
        quantStart =
          withLoc $
            choice
              [ (True,) <$ symbol TokQuantStartU,
                (False,) <$ symbol TokQuantStart
              ]

        quantEnd :: Parser (Bool, Loc)
        quantEnd =
          withLoc $
            choice
              [ (True,) <$ symbol TokQuantEndU,
                (False,) <$ symbol TokQuantEnd
              ]

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
      arrow <-
        withLoc $
          choice
            [ (False,) <$ symbol TokArrow <?> "->",
              (True,) <$ symbol TokArrowU <?> "→"
            ]
      return $ \x y -> TFunc x arrow y (x <--> y)

    term :: Parser Type
    term = ignoreIndentations $ do
      parensType <|> array <|> base <?> "type term"

    parensType :: Parser Type
    parensType = TParen <$> tokenParenStart <*> type' <*> tokenParenEnd

    base :: Parser Type
    base = do
      withLoc (TBase <$> extract isBaseType) <?> "base type"
      where
        isBaseType (TokUpperName "Int") = Just TInt
        isBaseType (TokUpperName "Bool") = Just TBool
        isBaseType (TokUpperName "Char") = Just TChar
        isBaseType _ = Nothing

    array :: Parser Type
    array = withLoc $ TArray <$ symbol TokArray <*> interval <* symbol TokOf <*> type'

    interval :: Parser Interval
    interval = withLoc $ do
      start <-
        choice
          [Excluding <$ symbol TokParenStart, Including <$ symbol TokBracketStart]
      i <- expression
      symbol TokRange
      j <- expression
      end <-
        choice
          [Excluding <$ symbol TokParenEnd, Including <$ symbol TokBracketEnd]
      return $ Interval (start i) (end j)

--------------------------------------------------------------------------------

-- | Combinators
block :: Parser a -> Parser a
block parser = do
  Util.ignore TokIndent <?> "indentation"
  result <- parser
  Util.ignore TokDedent <?> "dedentation"
  return result

block' :: Parser () -> Parser a -> Parser () -> Parser a
block' open parser close = do
  open
  symbol TokIndent <?> "indentation"
  result <- parser
  choice
    [ do
        -- the ideal case
        symbol TokDedent <?> "dedentation"
        close,
      do
        -- the fucked up case:
        --  the lexer is not capable of handling cases like "if True -> skip fi"
        --  because it's not possible to determine the number of `TokDedent` before `TokFi`
        close
        symbol TokDedent <?> "dedentation"
    ]
  return result

-- consumes 0 or more newlines/indents/dedents
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
  (_, start) <- Util.getLoc (symbol TokParenStart <?> "opening parenthesis")
  result <- parser
  (_, end) <- Util.getLoc (symbol TokParenEnd <?> "closing parenthesis")
  let loc = start <--> end
  return (result, loc)

braces :: Parser a -> Parser (a, Loc)
braces parser = do
  (_, start) <- Util.getLoc (symbol TokBraceStart <?> "opening braces")
  result <- parser
  (_, end) <- Util.getLoc (symbol TokBraceEnd <?> "closing braces")
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
