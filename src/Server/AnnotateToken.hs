{-# LANGUAGE LambdaCase #-}
module Server.AnnotateToken where

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

-- | All of the information we want to annotate on a Token 
newtype Annotation = Annotation
  { -- | Highlighting info 
    annoHighlighting :: [J.SemanticTokenAbsolute]
  }

toAnnotation
  :: Located a
  => J.SemanticTokenTypes
  -> [J.SemanticTokenModifiers]
  -> a
  -> Map Range Annotation
toAnnotation types modifiers x = case fromLoc (locOf x) of
  Nothing    -> mempty
  Just range -> Map.singleton range $ Annotation
    { annoHighlighting = [ J.SemanticTokenAbsolute
                             (posLine (rangeStart range) - 1)
                             (posCol (rangeStart range) - 1)
                             (rangeSpan range)
                             types
                             modifiers
                         ]
    }

--------------------------------------------------------------------------------

-- | Given a Concrete syntax node, returns a mapping of Range-Annotation
class Annotate a where
  annotate :: a -> Map Range Annotation

instance Annotate a => Annotate (Maybe a) where
  annotate Nothing  = mempty
  annotate (Just x) = annotate x

instance Annotate a => Annotate [a] where
  annotate = mconcat . map annotate

instance Annotate a => Annotate (SepBy tok a) where
  annotate = mconcat . map annotate . toList

instance (Annotate a, Annotate b) => Annotate (Either a b) where
  annotate (Left  a) = annotate a
  annotate (Right a) = annotate a


--------------------------------------------------------------------------------
-- newtypes for giving common datatype like `Name` different interpretations

newtype AsConstructor = AsConstructor Name

instance Annotate AsConstructor where
  annotate (AsConstructor a) = toAnnotation J.SttEnumMember [] a

newtype AsVariable = AsVariable Name

instance Annotate AsVariable where
  annotate (AsVariable a) = toAnnotation J.SttVariable [] a

newtype AsName = AsName Name

instance Annotate AsName where
  annotate (AsName a) = toAnnotation J.SttFunction [] a

--------------------------------------------------------------------------------
-- Program

instance Annotate Program where
  annotate (Program as bs) = annotate as <> annotate bs

--------------------------------------------------------------------------------
-- Definition

instance Annotate DefinitionBlock where
  annotate (DefinitionBlock _tokA as _tokB) = annotate as

instance Annotate Definition where
  annotate (TypeDefn tokData name binders _tokDef bs) =
    toAnnotation J.SttKeyword [] tokData
      <> toAnnotation J.SttType      [] name
      <> toAnnotation J.SttParameter [] binders
      <> annotate bs
  annotate (FuncDefnSig a b) = annotate a <> annotate b
  annotate (FuncDefn a bs _tok c) =
    toAnnotation J.SttFunction [J.StmDeclaration] a
      <> annotate (fmap AsVariable bs)
      <> annotate c

--------------------------------------------------------------------------------
-- Declaration

instance Annotate Declaration where
  annotate (ConstDecl tok a) = toAnnotation J.SttKeyword [] tok <> annotate a
  annotate (VarDecl   tok a) = toAnnotation J.SttKeyword [] tok <> annotate a

instance Annotate TypeDefnCtor where
  annotate (TypeDefnCtor name types) = annotate (AsName name) <> annotate types

instance Annotate DeclBase where
  annotate (DeclBase as _ b) = annotate (fmap AsVariable as) <> annotate b

instance Annotate DeclProp where
  annotate (DeclProp _tokA a _tokB) = annotate a
instance Annotate DeclType where
  annotate (DeclType a b) = annotate a <> annotate b

--------------------------------------------------------------------------------
-- Stmt

instance Annotate Stmt where
  annotate = \case
    Skip  x -> toAnnotation J.SttKeyword [] x
    Abort x -> toAnnotation J.SttKeyword [] x
    Assign as tok bs ->
      annotate (fmap AsVariable as)
        <> toAnnotation J.SttKeyword [] tok
        <> annotate bs
    AAssign a _ b _ tok c ->
      annotate (AsVariable a)
        <> toAnnotation J.SttKeyword [] tok
        <> annotate b
        <> annotate c
    Assert _ a _ -> annotate a
    LoopInvariant _ a _ tok _ b _ ->
      annotate a <> toAnnotation J.SttKeyword [] tok <> annotate b
    Do tokA as tokB ->
      toAnnotation J.SttKeyword [] tokA
        <> annotate as
        <> toAnnotation J.SttKeyword [] tokB
    If tokA as tokB ->
      toAnnotation J.SttKeyword [] tokA
        <> annotate as
        <> toAnnotation J.SttKeyword [] tokB
    SpecQM _ -> mempty
    Spec tokA _ tokB ->
      toAnnotation J.SttKeyword [] tokA <> toAnnotation J.SttKeyword [] tokB
    Proof tokA _ tokB ->
      toAnnotation J.SttKeyword [] tokA <> toAnnotation J.SttKeyword [] tokB
    Alloc a tok tokNew _ bs _ ->
      annotate (AsVariable a)
        <> toAnnotation J.SttKeyword []                  tok
        <> toAnnotation J.SttKeyword [J.StmModification] tokNew
        <> annotate bs
    HLookup a tok tokStar b ->
      annotate (AsVariable a)
        <> toAnnotation J.SttKeyword []                  tok
        <> toAnnotation J.SttKeyword [J.StmModification] tokStar
        <> annotate b
    HMutate tokStar a tok b ->
      toAnnotation J.SttKeyword [J.StmModification] tokStar
        <> annotate a
        <> toAnnotation J.SttKeyword [] tok
        <> annotate b
    Dispose tok a -> toAnnotation J.SttKeyword [] tok <> annotate a
    -- TODO:
    Block{}       -> mempty

instance Annotate GdCmd where
  annotate (GdCmd a tok bs) =
    annotate a <> toAnnotation J.SttMacro [] tok <> annotate bs

--------------------------------------------------------------------------------

instance Annotate Expr where
  annotate = \case
    Paren _ a _     -> annotate a
    Lit   a         -> annotate a
    Var   a         -> annotate (AsVariable a)
    Const a         -> annotate (AsVariable a)
    Op    a         -> toAnnotation J.SttOperator [] a
    Arr a _ b _     -> annotate a <> annotate b
    App a@App{}   b -> annotate a <> annotate b
    App (Const a) b -> annotate (AsName a) <> annotate b
    App a         b -> annotate a <> annotate b
    Quant tokA op names tokB a tokC b tokD ->
      toAnnotation J.SttKeyword [] tokA
        <> toAnnotation J.SttOperator [] op
        <> annotate (map AsVariable names)
        <> toAnnotation J.SttKeyword [] tokB
        <> annotate a
        <> toAnnotation J.SttKeyword [] tokC
        <> annotate b
        <> toAnnotation J.SttKeyword [] tokD
    Case tokA expr tokB cases ->
      toAnnotation J.SttKeyword [] tokA
        <> annotate expr
        <> toAnnotation J.SttKeyword [] tokB
        <> annotate cases

instance Annotate CaseConstructor where
  annotate (CaseConstructor ctor binders arrow body) =
    annotate (AsConstructor ctor)
      <> annotate (map AsVariable binders)
      <> toAnnotation J.SttMacro [] arrow
      <> annotate body

instance Annotate Lit where
  annotate x = toAnnotation J.SttNumber [] x

--------------------------------------------------------------------------------

instance Annotate EndpointOpen where
  annotate (IncludingOpening tok a) =
    toAnnotation J.SttKeyword [] tok <> annotate a
  annotate (ExcludingOpening tok a) =
    toAnnotation J.SttKeyword [] tok <> annotate a

instance Annotate EndpointClose where
  annotate (IncludingClosing a tok) =
    annotate a <> toAnnotation J.SttKeyword [] tok
  annotate (ExcludingClosing a tok) =
    annotate a <> toAnnotation J.SttKeyword [] tok

instance Annotate Interval where
  annotate (Interval a tok b) =
    annotate a <> toAnnotation J.SttKeyword [] tok <> annotate b

instance Annotate TBase where
  annotate = toAnnotation J.SttType []

instance Annotate Type where
  annotate (TParen _ a _) = annotate a
  annotate (TBase a     ) = annotate a
  annotate (TArray tokArray a tokOf b) =
    toAnnotation J.SttKeyword [] tokArray
      <> annotate a
      <> toAnnotation J.SttKeyword [] tokOf
      <> annotate b
  annotate (TFunc a tok b) =
    annotate a <> toAnnotation J.SttOperator [] tok <> annotate b
  -- TODO: handle user defined type annotate
  annotate TCon{}      = mempty
  annotate (TVar name) = toAnnotation J.SttType [] name
