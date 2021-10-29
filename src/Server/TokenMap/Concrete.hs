{-# LANGUAGE LambdaCase #-}
module Server.TokenMap.Concrete
  ( Highlighting, collectHighlighting )
 where

import           Data.Foldable                  ( toList )
import           Data.Loc                       ( Located(locOf)
                                                , posCol
                                                , posLine
                                                )
import           Data.Loc.Range
import           Data.Map                       ( Map )
import qualified Data.Map                      as Map
import qualified Language.LSP.Types            as J
import           Syntax.Common
import           Syntax.Concrete

collectHighlighting :: Program -> Map Range Highlighting
collectHighlighting = collect 

-- | All of the information we want to collect on a Token 
type Highlighting = J.SemanticTokenAbsolute

toHighlighting
  :: Located a
  => J.SemanticTokenTypes
  -> [J.SemanticTokenModifiers]
  -> a
  -> Map Range Highlighting
toHighlighting types modifiers x = case fromLoc (locOf x) of
  Nothing    -> mempty
  Just range -> Map.singleton range $  J.SemanticTokenAbsolute
                             (posLine (rangeStart range) - 1)
                             (posCol (rangeStart range) - 1)
                             (rangeSpan range)
                             types
                             modifiers

--------------------------------------------------------------------------------

-- | Given a Concrete syntax node, returns a mapping of Range-Highlighting
class CollectHighlighting a where
  collect :: a -> Map Range Highlighting

instance CollectHighlighting a => CollectHighlighting (Maybe a) where
  collect Nothing  = mempty
  collect (Just x) = collect x

instance CollectHighlighting a => CollectHighlighting [a] where
  collect = mconcat . map collect

instance CollectHighlighting a => CollectHighlighting (SepBy tok a) where
  collect = mconcat . map collect . toList

instance (CollectHighlighting a, CollectHighlighting b) => CollectHighlighting (Either a b) where
  collect (Left  a) = collect a
  collect (Right a) = collect a


--------------------------------------------------------------------------------
-- newtypes for giving common datatype like `Name` different interpretations

newtype AsConstructor = AsConstructor Name

instance CollectHighlighting AsConstructor where
  collect (AsConstructor a) = toHighlighting J.SttEnumMember [] a

newtype AsVariable = AsVariable Name

instance CollectHighlighting AsVariable where
  collect (AsVariable a) = toHighlighting J.SttVariable [] a

newtype AsName = AsName Name

instance CollectHighlighting AsName where
  collect (AsName a) = toHighlighting J.SttFunction [] a

--------------------------------------------------------------------------------
-- Program

instance CollectHighlighting Program where
  collect (Program as bs) = collect as <> collect bs

--------------------------------------------------------------------------------
-- Definition

instance CollectHighlighting DefinitionBlock where
  collect (DefinitionBlock _tokA as _tokB) = collect as

instance CollectHighlighting Definition where
  collect (TypeDefn tokData name binders _tokDef bs) =
    toHighlighting J.SttKeyword [] tokData
      <> toHighlighting J.SttType      [] name
      <> toHighlighting J.SttParameter [] binders
      <> collect bs
  collect (FuncDefnSig a b) = collect a <> collect b
  collect (FuncDefn a bs _tok c) =
    toHighlighting J.SttFunction [J.StmDeclaration] a
      <> collect (fmap AsVariable bs)
      <> collect c

--------------------------------------------------------------------------------
-- Declaration

instance CollectHighlighting Declaration where
  collect (ConstDecl tok a) = toHighlighting J.SttKeyword [] tok <> collect a
  collect (VarDecl   tok a) = toHighlighting J.SttKeyword [] tok <> collect a

instance CollectHighlighting TypeDefnCtor where
  collect (TypeDefnCtor name types) = collect (AsName name) <> collect types

instance CollectHighlighting DeclBase where
  collect (DeclBase as _ b) = collect (fmap AsVariable as) <> collect b

instance CollectHighlighting DeclProp where
  collect (DeclProp _tokA a _tokB) = collect a
instance CollectHighlighting DeclType where
  collect (DeclType a b) = collect a <> collect b

--------------------------------------------------------------------------------
-- Stmt

instance CollectHighlighting Stmt where
  collect = \case
    Skip  x -> toHighlighting J.SttKeyword [] x
    Abort x -> toHighlighting J.SttKeyword [] x
    Assign as tok bs ->
      collect (fmap AsVariable as)
        <> toHighlighting J.SttKeyword [] tok
        <> collect bs
    AAssign a _ b _ tok c ->
      collect (AsVariable a)
        <> toHighlighting J.SttKeyword [] tok
        <> collect b
        <> collect c
    Assert _ a _ -> collect a
    LoopInvariant _ a _ tok _ b _ ->
      collect a <> toHighlighting J.SttKeyword [] tok <> collect b
    Do tokA as tokB ->
      toHighlighting J.SttKeyword [] tokA
        <> collect as
        <> toHighlighting J.SttKeyword [] tokB
    If tokA as tokB ->
      toHighlighting J.SttKeyword [] tokA
        <> collect as
        <> toHighlighting J.SttKeyword [] tokB
    SpecQM _ -> mempty
    Spec tokA _ tokB ->
      toHighlighting J.SttKeyword [] tokA <> toHighlighting J.SttKeyword [] tokB
    Proof tokA _ tokB ->
      toHighlighting J.SttKeyword [] tokA <> toHighlighting J.SttKeyword [] tokB
    Alloc a tok tokNew _ bs _ ->
      collect (AsVariable a)
        <> toHighlighting J.SttKeyword []                  tok
        <> toHighlighting J.SttKeyword [J.StmModification] tokNew
        <> collect bs
    HLookup a tok tokStar b ->
      collect (AsVariable a)
        <> toHighlighting J.SttKeyword []                  tok
        <> toHighlighting J.SttKeyword [J.StmModification] tokStar
        <> collect b
    HMutate tokStar a tok b ->
      toHighlighting J.SttKeyword [J.StmModification] tokStar
        <> collect a
        <> toHighlighting J.SttKeyword [] tok
        <> collect b
    Dispose tok a -> toHighlighting J.SttKeyword [] tok <> collect a
    -- TODO:
    Block{}       -> mempty

instance CollectHighlighting GdCmd where
  collect (GdCmd a tok bs) =
    collect a <> toHighlighting J.SttMacro [] tok <> collect bs

--------------------------------------------------------------------------------

instance CollectHighlighting Expr where
  collect = \case
    Paren _ a _     -> collect a
    Lit   a         -> collect a
    Var   a         -> collect (AsVariable a)
    Const a         -> collect (AsVariable a)
    Op    a         -> toHighlighting J.SttOperator [] a
    Arr a _ b _     -> collect a <> collect b
    App a@App{}   b -> collect a <> collect b
    App (Const a) b -> collect (AsName a) <> collect b
    App a         b -> collect a <> collect b
    Quant tokA op names tokB a tokC b tokD ->
      toHighlighting J.SttKeyword [] tokA
        <> toHighlighting J.SttOperator [] op
        <> collect (map AsVariable names)
        <> toHighlighting J.SttKeyword [] tokB
        <> collect a
        <> toHighlighting J.SttKeyword [] tokC
        <> collect b
        <> toHighlighting J.SttKeyword [] tokD
    Case tokA expr tokB cases ->
      toHighlighting J.SttKeyword [] tokA
        <> collect expr
        <> toHighlighting J.SttKeyword [] tokB
        <> collect cases

instance CollectHighlighting CaseConstructor where
  collect (CaseConstructor ctor binders arrow body) =
    collect (AsConstructor ctor)
      <> collect (map AsVariable binders)
      <> toHighlighting J.SttMacro [] arrow
      <> collect body

instance CollectHighlighting Lit where
  collect x = toHighlighting J.SttNumber [] x

--------------------------------------------------------------------------------

instance CollectHighlighting EndpointOpen where
  collect (IncludingOpening tok a) =
    toHighlighting J.SttKeyword [] tok <> collect a
  collect (ExcludingOpening tok a) =
    toHighlighting J.SttKeyword [] tok <> collect a

instance CollectHighlighting EndpointClose where
  collect (IncludingClosing a tok) =
    collect a <> toHighlighting J.SttKeyword [] tok
  collect (ExcludingClosing a tok) =
    collect a <> toHighlighting J.SttKeyword [] tok

instance CollectHighlighting Interval where
  collect (Interval a tok b) =
    collect a <> toHighlighting J.SttKeyword [] tok <> collect b

instance CollectHighlighting TBase where
  collect = toHighlighting J.SttType []

instance CollectHighlighting Type where
  collect (TParen _ a _) = collect a
  collect (TBase a     ) = collect a
  collect (TArray tokArray a tokOf b) =
    toHighlighting J.SttKeyword [] tokArray
      <> collect a
      <> toHighlighting J.SttKeyword [] tokOf
      <> collect b
  collect (TFunc a tok b) =
    collect a <> toHighlighting J.SttOperator [] tok <> collect b
  -- TODO: handle user defined type collect
  collect TCon{}      = mempty
  collect (TVar name) = toHighlighting J.SttType [] name
