{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}

module Server.Handler.Highlighting where

import           Data.Loc.Range
import qualified Language.LSP.Types            as J


import           Server.Stab
import           Syntax.Concrete

import           Data.Loc

import           Error                          ( Error )
import           Server.DSL
import           Server.Interpreter.RealWorld
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
handler uri responder = do
  case J.uriToFilePath uri of
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

toToken
  :: Ranged a
  => J.SemanticTokenTypes
  -> [J.SemanticTokenModifiers]
  -> a
  -> J.SemanticTokenAbsolute
toToken types modifiers x =
  let range = rangeOf x
  in  J.SemanticTokenAbsolute (posLine (rangeStart range) - 1)
                              (posCol (rangeStart range) - 1)
                              (rangeSpan range)
                              types
                              modifiers

instance Collect Program J.SemanticTokenAbsolute where
  collect (Program _ _) = []

-- instance Collect Definition J.SemanticTokenAbsolute where
--   collect (ProcDefn name args _tokDefn process) =
--     [toToken J.SttFunction [J.StmDefinition] name]
--       <> map (toToken J.SttVariable []) args
--       <> [toToken J.SttFunction [J.StmDefinition] name]
--       <> collect process
--   collect (TypeSign name _tokHasType typ) =
--     [toToken J.SttInterface [J.StmDeclaration] name] <> collect typ

-- instance Collect Process J.SemanticTokenAbsolute where
--   collect = \case
--     Call name -> [toToken J.SttFunction [] name]
--     Link x tokLink y ->
--       [ toToken J.SttVariable [] x
--       , toToken J.SttOperator [] tokLink
--       , toToken J.SttVariable [] y
--       ]
--     Output x tokBrOpen y tokBrClose tokSeq _ a tokComp b _ ->
--       [ toToken J.SttVariable [] x
--         , toToken J.SttOperator [] tokBrOpen
--         , toToken J.SttVariable [] y
--         , toToken J.SttOperator [] tokBrClose
--         , toToken J.SttOperator [] tokSeq
--         ]
--         <> collect a
--         <> [toToken J.SttOperator [] tokComp]
--         <> collect b
--     Input x tokPrOpen y tokPrClose tokSeq a ->
--       [ toToken J.SttVariable [] x
--         , toToken J.SttOperator [] tokPrOpen
--         , toToken J.SttVariable [] y
--         , toToken J.SttOperator [] tokPrClose
--         , toToken J.SttOperator [] tokSeq
--         ]
--         <> collect a
--     Compose tokScope x tokSeq _ a tokComp b _ ->
--       [ toToken J.SttVariable [] x
--         , toToken J.SttOperator [] tokScope
--         , toToken J.SttOperator [] tokSeq
--         ]
--         <> collect a
--         <> [toToken J.SttOperator [] tokComp]
--         <> collect b
--     EmptyOutput x tokBrOpen tokBrClose tokSeq tokEnd ->
--       [ toToken J.SttVariable [] x
--         , toToken J.SttOperator [] tokBrOpen
--         , toToken J.SttOperator [] tokBrClose
--         , toToken J.SttOperator [] tokSeq
--         , toToken J.SttKeyword  [] tokEnd
--         ]
--     EmptyInput x tokPrOpen tokPrClose tokSeq a ->
--       [ toToken J.SttVariable [] x
--         , toToken J.SttOperator [] tokPrOpen
--         , toToken J.SttOperator [] tokPrClose
--         , toToken J.SttOperator [] tokSeq
--         ]
--         <> collect a
--     End tokEnd -> [toToken J.SttKeyword  [] tokEnd]

-- instance Collect Type J.SemanticTokenAbsolute where
--   collect x = [toToken J.SttType [] x]
