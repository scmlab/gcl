{-# LANGUAGE OverloadedStrings #-}

module Syntax.Parser where

import Control.Monad.Combinators.Expr
import Control.Monad.State (lift)
import Control.Monad (void)
import Data.Text.Lazy (Text)
import Data.Loc
import Data.Void
import Data.Foldable (fold)
import Text.Megaparsec hiding (Pos, State, ParseError, parse)
import qualified Text.Megaparsec as Mega

import Syntax.Concrete hiding (unary, binary)
import Syntax.Location ()
import Syntax.Parser.Lexer
import Syntax.Parser.Util (PosLog, extract)
import qualified Syntax.Parser.Util as Util

import Prelude hiding (Ordering(..))

--------------------------------------------------------------------------------
-- | States for source location bookkeeping

type Parser = ParsecT Void TokStream (PosLog Tok)
type SyntacticError = (Loc, String)

parse :: Parser a -> FilePath -> TokStream -> Either [SyntacticError] a
parse parser filepath tokenStream = do
  case Util.runPosLog (runParserT parser filepath tokenStream) of
    Left e -> Left (fromParseErrorBundle e)
    Right x -> Right x

  where
    fromParseErrorBundle :: ShowErrorComponent e
                       => ParseErrorBundle TokStream e
                       -> [SyntacticError]
    fromParseErrorBundle (ParseErrorBundle errors posState)
      = snd $ foldr f (posState, []) errors
      where
        f :: ShowErrorComponent e
          => Mega.ParseError TokStream e
          -> (PosState TokStream, [SyntacticError])
          -> (PosState TokStream, [SyntacticError])
        f err (initial, accum) =
            let (_, next) = reachOffset (errorOffset err) initial
            in (next, (getLoc err, parseErrorTextPretty err):accum)

        getLoc :: ShowErrorComponent e
          => Mega.ParseError TokStream e
          -> Loc
        -- get the Loc of all unexpected tokens
        getLoc (TrivialError _ (Just (Tokens xs)) _) = fold $ fmap locOf xs
        getLoc _ = mempty

program :: Parser Program
program = withLoc $ do
  ignoreNewlines
  declarations <- many declaration <?> "declarations"
  stmts <- statements <?> "statements"
  eof
  return $ Program declarations stmts

specContent :: Parser [Stmt]
specContent = do
  ignoreNewlines
  statements <?> "statements"

--------------------------------------------------------------------------------
-- | Stmts

statement :: Parser Stmt
statement = choice
  [ try assign
  , abort
  , try assertWithBnd
  , spec
  , assert
  , skip
  , repetition
  , selection
  , hole
  ] <?> "statement"


statements :: Parser [Stmt]
statements = try statements1 <|> return []

statements1 :: Parser [Stmt]
statements1 = do
  stmt <- statement
  rest <- choice
    [ do
        expectLineEnding <?> "a newline or a semicolon"
        try statements <|> return []
    , return []
    ]
  return (stmt:rest)

expectLineEnding :: Parser ()
expectLineEnding = choice [withSemicolon, withoutSemicolon]
  where
    withoutSemicolon = expectNewline
    withSemicolon = do
      -- see if the latest accepcted token is TokSemi
      t <- lift Util.getLastToken
      case t of
        Just TokSemi -> return ()
        _ -> void $ do
          Util.ignore TokSemi
          void $ many (Util.ignore TokNewline)

skip :: Parser Stmt
skip = withLoc $ Skip <$ symbol TokSkip

abort :: Parser Stmt
abort = withLoc $ Abort <$ symbol TokAbort

assert :: Parser Stmt
assert = withLoc $ Assert <$> braces predicate

assertWithBnd :: Parser Stmt
assertWithBnd = withLoc $ braces $ LoopInvariant
  <$> predicate
  <*  (symbol TokComma  <?> "comma")
  <*  (symbol TokBnd    <?> "bnd")
  <*  (symbol TokSemi   <?> "semicolon")
  <*> expression

assign :: Parser Stmt
assign = withLoc $
  Assign  <$> variableList
          <*  (symbol TokAssign <?> ":=")
          <*> expressionList

repetition :: Parser Stmt
repetition = withLoc $
  Do  <$  (symbol TokDo <?> "do")
      <*> guardedCommands
      <*  (symbol TokOd <?> "od")

selection :: Parser Stmt
selection = withLoc $
  If  <$  (symbol TokIf <?> "if")
      <*> guardedCommands
      <*  (symbol TokFi <?> "fi")

guardedCommands :: Parser [GdCmd]
guardedCommands = sepBy1 guardedCommand (symbol TokGuardBar <?> "|")

guardedCommand :: Parser GdCmd
guardedCommand = withLoc $
  GdCmd <$> predicate
        <*  (symbol TokArrow <?> "->")
        <*> statements1

hole :: Parser Stmt
hole = withLoc $ SpecQM <$ (symbol TokQM <?> "?")

spec :: Parser Stmt
spec = withLoc $ do
  symbol TokSpecStart <?> "{!"
  expectNewline <?> "<newline> after a the start of a Spec"
  _ <- specContent
  _ <- takeWhileP (Just "anything other than '!}'") isTokSpecEnd
  symbol TokSpecEnd <?> "!}"
  expectNewline <?> "<newline> after a the end of a Spec"

  return $ Spec

  where
    isTokSpecEnd :: L Tok -> Bool
    isTokSpecEnd (L _ TokSpecEnd) = False
    isTokSpecEnd _ = True

--------------------------------------------------------------------------------
-- | Expressions

expressionList :: Parser [Expr]
expressionList = sepBy1 expression (symbol TokComma) <?> "a list of expressions separated by commas"

predicate :: Parser Expr
predicate = expression <?> "predicate"

expression :: Parser Expr
expression = makeExprParser term table <?> "expression"
  where
    table :: [[Operator Parser Expr]]
    table = [ [ Postfix application ]


            , [ InfixL $ binary Mod  TokMod
              ]
            , [ InfixL $ binary Mul  TokMul
              , InfixL $ binary Div  TokDiv
              ]
            , [ InfixL $ binary Add  TokAdd
              , InfixL $ binary Sub  TokSub
              ]

            , [ InfixN $ binary NEQ TokNEQ
              , InfixN $ binary LT  TokLT
              , InfixN $ binary LTE TokLTE
              , InfixN $ binary GT  TokGT
              , InfixN $ binary GTE TokGTE
              ]

            , [ InfixN $ binary EQ  TokEQ
              ]

            , [ Prefix $ unary  Neg  TokNeg     ]
            , [ InfixL $ binary Conj TokConj    ]
            , [ InfixL $ binary Disj TokDisj    ]
            , [ InfixR $ binary Implies TokImpl ]


            ]

    application :: Parser (Expr -> Expr)
    application =  do
      terms <- many term
      return $ \func -> do
        let app inner t = App inner t (func <--> t)
        foldl app func terms

    unary :: Op -> Tok -> Parser (Expr -> Expr)
    unary operator' tok = do
      (op, loc) <- Util.getLoc (operator' <$ symbol tok)
      return $ \result -> App (Op op loc) result (loc <--> result)

    binary :: Op -> Tok -> Parser (Expr -> Expr -> Expr)
    binary operator' tok = do
      (op, loc) <- Util.getLoc (operator' <$ symbol tok)
      return $ \x y -> App (App (Op op loc) x (x <--> loc)) y (x <--> y)


    term :: Parser Expr
    term = try term' <|> parens expression
      where
        term' :: Parser Expr
        term' = withLoc (choice
          [ Var    <$> lower
          , Const  <$> upper
          , Lit    <$> literal
          , Op     <$ symbol TokParenStart <*> operator <* symbol TokParenEnd
          , Quant  <$  symbol TokBracketStart
                   <*> term
                   <*> some lower
                   <*  symbol TokColon
                   <*> expression
                   <*  symbol TokColon
                   <*> expression
                   <*  symbol TokBracketEnd
          , Hole   <$  symbol TokQM
          ]) <?> "term"


    literal :: Parser Lit
    literal = choice
      [ Bol True  <$  symbol TokTrue
      , Bol False <$  symbol TokFalse
      , Num       <$> integer
      ] <?> "literal"

    operator :: Parser Op
    operator =  choice
            [ EQ      <$  symbol TokEQ
            , NEQ     <$  symbol TokNEQ
            , LTE     <$  symbol TokLTE
            , GTE     <$  symbol TokGTE
            , LT      <$  symbol TokLT
            , GT      <$  symbol TokGT
            , Implies <$  symbol TokArrow
            , Conj    <$  symbol TokConj
            , Disj    <$  symbol TokDisj
            , Neg     <$  symbol TokNeg
            , Add     <$  symbol TokAdd
            , Sub     <$  symbol TokSub
            , Mul     <$  symbol TokMul
            , Div     <$  symbol TokDiv
            , Mod     <$  symbol TokMod
            ] <?> "operator"

    -- op :: Parser Expr
    -- op = withLoc (Op <$> choice
    --         [ EQ    <$  symbol TokEQ
    --         , Add   <$  symbol TokAdd
    --         ] <?> "operator")

--------------------------------------------------------------------------------
-- | Type

type' :: Parser Type
type' = makeExprParser term table <?> "type"
  where
    table :: [[Operator Parser Type]]
    table = [ [ InfixR function ]
            ]

    function :: Parser (Type -> Type -> Type)
    function = do
      symbol TokArrow <?> "->"
      return $ \x y -> TFunc x y (x <--> y)

    term :: Parser Type
    term = parens type' <|> array <|> base <?> "type term"

    base :: Parser Type
    base = withLoc (TBase <$> extract isBaseType) <?> "base type"
      where
        isBaseType (TokUpperName "Int") = Just TInt
        isBaseType (TokUpperName "Bool") = Just TBool
        isBaseType (TokUpperName "Char") = Just TChar
        isBaseType _ = Nothing

    array :: Parser Type
    array = withLoc $ do
      symbol TokArray
      i <- interval
      symbol TokOf
      t <- type'
      return $ TArray i t

    interval :: Parser Interval
    interval = withLoc $ do
      start <- choice [ Excluding <$ symbol TokParenStart
                      , Including <$ symbol TokBracketStart
                      ]
      i <- expression
      symbol TokRange
      j <- expression
      end <- choice [ Excluding <$ symbol TokParenEnd
                    , Including <$ symbol TokBracketEnd
                    ]
      return $ Interval (start i) (end j)

--------------------------------------------------------------------------------
-- | Declarations

declaration :: Parser Declaration
declaration = choice
  [ constantDecl
  , variableDecl
  ] <?> "declaration"

constantDecl :: Parser Declaration
constantDecl = withLoc $ do
  symbol TokCon
  types <- constList
  symbol TokColon <?> "colon"
  t <- type'
  expectNewline <?> "<newline> after a declaration"
  return $ ConstDecl types t

variableDecl :: Parser Declaration
variableDecl = withLoc $ do
  symbol TokVar
  vars <- variableList
  symbol TokColon <?> "colon"
  t <- type'
  expectNewline <?> "<newline> after a declaration"
  return $ VarDecl vars t

--------------------------------------------------------------------------------
-- | Variables and stuff

-- constant :: Parser Expr
-- constant = withLoc (Const <$> upper) <?> "constant"

-- separated by commas
constList :: Parser [Upper]
constList =
  (sepBy1 upper (symbol TokComma <?> "comma")
    <?> "a list of constants separated by commas")
      <*  ignoreNewlines

-- variable :: Parser Expr
-- variable = withLoc (Var <$> lower) <?> "variable"

-- separated by commas
variableList :: Parser [Lower]
variableList =
    (sepBy1 lower (symbol TokComma <?> "comma")
      <?> "a list of variables separated by commas")
        <*  ignoreNewlines

--------------------------------------------------------------------------------
-- | Combinators

-- consumes 0 or more newlines
ignoreNewlines :: Parser ()
ignoreNewlines = void $ many (Util.ignore TokNewline)

expectNewline :: Parser ()
expectNewline = do
  -- see if the latest accepcted token is TokNewline
  t <- lift Util.getLastToken
  case t of
    Just TokNewline -> return ()
    _ -> void $ some (Util.ignore TokNewline)

symbol :: Tok -> Parser ()
symbol t = do
  Util.symbol t
  ignoreNewlines

withLoc :: Parser (Loc -> a) -> Parser a
withLoc = Util.withLoc

-- -- followed by at least 1 newline
-- withLocStmt :: Parser (Loc -> a) -> Parser a
-- withLocStmt p = do
--   result <- Util.withLoc p
--   expectNewline <?> "<newline> after a statement"
--   return result

parens :: Relocatable a => Parser a -> Parser a
parens = Util.between
  (symbol TokParenStart <?> "left parenthesis")
  (symbol TokParenEnd <?> "right parenthesis")

braces :: Parser a -> Parser a
braces = between
  (symbol TokBraceStart <?> "left brace")
  (symbol TokBraceEnd <?> "right brace")

upperName :: Parser Text
upperName = extract p
  where
    p (TokUpperName s) = Just s
    p _ = Nothing

upper :: Parser Upper
upper = withLoc (Upper <$> upperName) <?> "identifier that starts with a uppercase letter"

lowerName :: Parser Text
lowerName = extract p
  where
    p (TokLowerName s) = Just s
    p _ = Nothing

lower :: Parser Lower
lower = withLoc (Lower <$> lowerName) <?> "identifier that starts with a lowercase letter"

integer :: Parser Int
integer = extract p <?> "integer"
  where
    p (TokInt s) = Just s
    p _ = Nothing
