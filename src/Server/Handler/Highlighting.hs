{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}

module Server.Handler.Highlighting where

import           Data.Loc.Range
import qualified Language.LSP.Types            as J


import           Server.Stab
import           Syntax.Concrete

import           Data.Loc                hiding ( fromLoc )

import           Data.Foldable                  ( toList )
import           Data.List                      ( sort )
import           Error                          ( Error )
import           Server.DSL
import           Server.Interpreter.RealWorld
import           Syntax.Common                  ( Name )
import           Syntax.Parser                  ( pProgram )

ignoreErrors
  :: Either [Error] (Either J.ResponseError (Maybe J.SemanticTokens))
  -> Either J.ResponseError (Maybe J.SemanticTokens)
ignoreErrors (Left  _errors) = Right Nothing
ignoreErrors (Right xs     ) = xs

handler
  :: J.Uri
  -> (Either J.ResponseError (Maybe J.SemanticTokens) -> ServerM ())
  -> ServerM ()
handler uri responder = case J.uriToFilePath uri of
  Nothing       -> return ()
  Just filepath -> do
    interpret filepath (responder . ignoreErrors) $ do
      source  <- getSource
      program <- parse pProgram source
      let legend = J.SemanticTokensLegend
            (J.List J.knownSemanticTokenTypes)
            (J.List J.knownSemanticTokenModifiers)
      let tokens = J.makeSemanticTokens legend (collect program)
      case tokens of
        Left t -> return $ Left $ J.ResponseError J.InternalError t Nothing
        Right tokens' -> return $ Right $ Just tokens'

--------------------------------------------------------------------------------
-- Helper functions for converting stuff to SemanticTokenAbsolute

toToken
  :: Ranged a
  => J.SemanticTokenTypes
  -> [J.SemanticTokenModifiers]
  -> a
  -> [J.SemanticTokenAbsolute]
toToken types modifiers x =
  let range = rangeOf x
  in  [ J.SemanticTokenAbsolute (posLine (rangeStart range) - 1)
                                (posCol (rangeStart range) - 1)
                                (rangeSpan range)
                                types
                                modifiers
      ]

toToken'
  :: Located a
  => J.SemanticTokenTypes
  -> [J.SemanticTokenModifiers]
  -> a
  -> [J.SemanticTokenAbsolute]
toToken' types modifiers x = case fromLoc (locOf x) of
  Nothing -> []
  Just range ->
    [ J.SemanticTokenAbsolute (posLine (rangeStart range) - 1)
                              (posCol (rangeStart range) - 1)
                              (rangeSpan range)
                              types
                              modifiers
    ]

--------------------------------------------------------------------------------
-- newtypes for giving common datatype like `Name` different interpretations

newtype AsVariable = AsVariable Name

instance Collect AsVariable J.SemanticTokenAbsolute where
  collect (AsVariable a) = toToken' J.SttVariable [] a

newtype AsName = AsName Name

instance Collect AsName J.SemanticTokenAbsolute where
  collect (AsName a) = toToken' J.SttFunction [] a

--------------------------------------------------------------------------------
-- Program 

instance Collect Program J.SemanticTokenAbsolute where
  collect (Program as bs) = (as >>= collect) <> (bs >>= collect)

--------------------------------------------------------------------------------
-- Definition  

instance Collect DefinitionBlock J.SemanticTokenAbsolute where
  collect (DefinitionBlock _tokA as _tokB) = toList as >>= collect

instance Collect Definition J.SemanticTokenAbsolute where
  collect (TypeDefn tokData name binders _tokDef bs) =
    toToken J.SttKeyword [] tokData
      <> toToken' J.SttType      [] name
      <> toToken' J.SttParameter [] binders
      <> (toList bs >>= collect)
  collect (FuncDefnSig a b) = collect a <> collect b
  collect (FuncDefn a bs _tok c) =
    toToken' J.SttFunction [J.StmDeclaration] a
      <> (bs >>= collect . AsVariable)
      <> collect c

--------------------------------------------------------------------------------
-- Declaration

instance Collect Declaration J.SemanticTokenAbsolute where
  collect (ConstDecl tok a) = toToken J.SttKeyword [] tok <> collect a
  collect (VarDecl   tok a) = toToken J.SttKeyword [] tok <> collect a

instance Collect TypeDefnCtor J.SemanticTokenAbsolute where
  collect (TypeDefnCtor name types) =
    collect (AsName name) <> (types >>= collect)

instance Collect DeclBase J.SemanticTokenAbsolute where
  collect (DeclBase as _ b) = (toList as >>= collect . AsVariable) <> collect b

instance Collect DeclProp J.SemanticTokenAbsolute where
  collect (DeclProp _tokA a _tokB) = collect a
instance Collect DeclType J.SemanticTokenAbsolute where
  collect (DeclType a b) = collect a <> collect b

--------------------------------------------------------------------------------
-- Stmt

instance Collect Stmt J.SemanticTokenAbsolute where
  collect = \case
    Skip  x -> toToken' J.SttKeyword [] x
    Abort x -> toToken' J.SttKeyword [] x
    Assign as tok bs ->
      (toList as >>= collect . AsVariable)
        <> toToken' J.SttKeyword [] tok
        <> (toList bs >>= collect)
    AAssign a _ b _ tok c ->
      collect (AsVariable a)
        <> toToken' J.SttKeyword [] tok
        <> collect b
        <> collect c
    Assert _ a _ -> collect a
    LoopInvariant _ a _ tok _ b _ ->
      collect a <> toToken' J.SttKeyword [] tok <> collect b
    Do tokA as tokB ->
      toToken' J.SttKeyword [] tokA
        <> (toList as >>= collect)
        <> toToken' J.SttKeyword [] tokB
    If tokA as tokB ->
      toToken' J.SttKeyword [] tokA
        <> (toList as >>= collect)
        <> toToken' J.SttKeyword [] tokB
    SpecQM _ -> []
    Spec tokA _ tokB ->
      toToken' J.SttKeyword [] tokA <> toToken' J.SttKeyword [] tokB
    Proof tokA _ tokB ->
      toToken' J.SttKeyword [] tokA <> toToken' J.SttKeyword [] tokB
    Alloc a tok tokNew _ bs _ ->
      collect (AsVariable a)
        <> toToken' J.SttKeyword []                  tok
        <> toToken' J.SttKeyword [J.StmModification] tokNew
        <> (toList bs >>= collect)
    HLookup a tok tokStar b ->
      collect (AsVariable a)
        <> toToken' J.SttKeyword []                  tok
        <> toToken' J.SttKeyword [J.StmModification] tokStar
        <> collect b
    HMutate tokStar a tok b ->
      toToken' J.SttKeyword [J.StmModification] tokStar
        <> collect a
        <> toToken' J.SttKeyword [] tok
        <> collect b
    Dispose tok a -> toToken' J.SttKeyword [] tok <> collect a
    -- TODO:
    Block{}       -> []

instance Collect GdCmd J.SemanticTokenAbsolute where
  collect (GdCmd a tok bs) =
    collect a <> toToken' J.SttMacro [] tok <> (bs >>= collect)

--------------------------------------------------------------------------------
-- Expr

instance Collect Expr J.SemanticTokenAbsolute where
  collect = \case
    Paren _ a _     -> collect a
    Lit   a         -> collect a
    Var   a         -> collect (AsVariable a)
    Const a         -> collect (AsVariable a)
    Op    a         -> toToken' J.SttOperator [] a
    --Chain a b c -> collect a <> toToken' J.SttOperator [] b <> collect c
    Arr a _ b _     -> collect a <> collect b
    -- NOTE: sorting is need here, because:
    --  1. the client will ignore tokens that are out of order
    --  2. `App` may create tokens that are out of order
    --      (e.g. "1 +" will be parsed as `App + 1`)
    App a@App{}   b -> sort $ collect a <> collect b
    App (Const a) b -> sort $ collect (AsName a) <> collect b
    App a         b -> sort $ collect a <> collect b
    Quant tokA op names tokB a tokC b tokD ->
      toToken' J.SttKeyword [] tokA
        <> toToken' J.SttOperator [] op
        <> (names >>= collect . AsVariable)
        <> toToken' J.SttKeyword [] tokB
        <> collect a
        <> toToken' J.SttKeyword [] tokC
        <> collect b
        <> toToken' J.SttKeyword [] tokD
    Case tokA expr tokB _ cases _ ->
      sort
        $  toToken' J.SttKeyword [] tokA
        <> collect expr
        <> toToken' J.SttKeyword [] tokB
        <> (toList cases >>= collect)

instance Collect Case J.SemanticTokenAbsolute where
  collect (CaseConstructor ctor binders arrow body) =
    sort
      $  toToken' J.SttEnumMember [] ctor
      <> (binders >>= collect . AsVariable)
      <> toToken' J.SttMacro [] arrow
      <> collect body

instance Collect Lit J.SemanticTokenAbsolute where
  collect = toToken' J.SttNumber []

--------------------------------------------------------------------------------
-- Pattern 

instance Collect Pattern J.SemanticTokenAbsolute where
  collect (PattParen _ a _ ) = collect a
  collect (PattBinder   a  ) = collect (AsVariable a)
  collect (PattWildcard tok) = toToken J.SttKeyword [] tok
  collect (PattConstructor a bs) =
    toToken' J.SttEnumMember [] a <> (bs >>= collect)

--------------------------------------------------------------------------------
-- Type

instance Collect EndpointOpen J.SemanticTokenAbsolute where
  collect (IncludingOpening tok a) = toToken J.SttKeyword [] tok <> collect a
  collect (ExcludingOpening tok a) = toToken J.SttKeyword [] tok <> collect a

instance Collect EndpointClose J.SemanticTokenAbsolute where
  collect (IncludingClosing a tok) = collect a <> toToken J.SttKeyword [] tok
  collect (ExcludingClosing a tok) = collect a <> toToken J.SttKeyword [] tok

instance Collect Interval J.SemanticTokenAbsolute where
  collect (Interval a tok b) =
    collect a <> toToken J.SttKeyword [] tok <> collect b

instance Collect TBase J.SemanticTokenAbsolute where
  collect = toToken' J.SttType []

instance Collect Type J.SemanticTokenAbsolute where
  collect (TParen _ a _) = collect a
  collect (TBase a     ) = collect a
  collect (TArray tokArray a tokOf b) =
    toToken J.SttKeyword [] tokArray
      <> collect a
      <> toToken J.SttKeyword [] tokOf
      <> collect b
  collect (TFunc a tok b) =
    collect a <> toToken J.SttOperator [] tok <> collect b
  -- TODO: handle user defined type collect
  collect TCon{}      = mempty
  collect (TVar name) = toToken' J.SttType [] name
