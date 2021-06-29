{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}

module Test.Substitution
  ( tests
  ) where

import           Data.Map                       ( Map )
import qualified Data.Map                      as Map
import           GCL.Predicate                  ( PO(PO)
                                                , Pred(..)
                                                )
import           Pretty                         ( (<+>)
                                                , Pretty(..)
                                                , indent
                                                , line
                                                , vcat
                                                )
import           Render                         ( Inlines
                                                , Render(render)
                                                )
import           Server.DSL                     ( parseProgram
                                                , sweep
                                                )
import           Server.Interpreter.Test        ( runTest
                                                , serializeTestResultValueOnly
                                                )
import           Syntax.Abstract                ( Expr(..) )
import           Test.Server                    ( runGoldenTest )
import           Test.Tasty              hiding ( after )

tests :: TestTree
tests = testGroup "Substitution" [letBindings]


letBindings :: TestTree
letBindings = testGroup "Substitute let-bindings" [run "let-1" "let-1.gcl"]
 where
  run :: String -> FilePath -> TestTree
  run = runGoldenTest "Substitution/assets/" $ \sourcePath source -> do
    return $ serializeTestResultValueOnly $ runTest sourcePath source $ do
      program        <- parseProgram source
      (pos, _, _, _) <- sweep program
      let substs = pos >>= extractSubst
      return (Right substs)

-- datatype for representing a SUBSTITUTION
data SUBST = SUBST Inlines -- BEFORE
                           (Map Inlines SUBST) -- next possible transitions
  deriving Show

instance Pretty SUBST where
  pretty (SUBST before next) = if Map.null next
    then "\"" <> pretty before <> "\""
    else "\"" <> pretty before <> "\"" <> line <> indent 4 (vcat next')
   where
    next' = map (\(k, x) -> "\"" <> pretty k <> "\" =>" <+> pretty x
            -- "\"" <> pretty k <> "\" =>" <> line <> indent 4 (vcat (map pretty xs))
                                                                    )
      $ Map.toList next


--------------------------------------------------------------------------------
-- | Typeclass for extracting `Subst` from the syntax tree 

class ExtractSubst a where
  extractSubst :: a -> [SUBST]

instance ExtractSubst PO where
  extractSubst (PO _ pre post _) = extractSubst pre <> extractSubst post

instance ExtractSubst Pred where
  extractSubst = \case
    Constant x          -> extractSubst x
    GuardIf   x _       -> extractSubst x
    GuardLoop x _       -> extractSubst x
    Assertion x _       -> extractSubst x
    LoopInvariant x y _ -> extractSubst x <> extractSubst y
    Bound x _           -> extractSubst x
    Conjunct xs         -> xs >>= extractSubst
    Disjunct xs         -> xs >>= extractSubst
    Negate   x          -> extractSubst x

instance ExtractSubst Expr where
  extractSubst = \case
    Paren x _                   -> extractSubst x
    Chain x _ y _               -> extractSubst x <> extractSubst y
    App x y _                   -> extractSubst x <> extractSubst y
    Lam _ x _                   -> extractSubst x
    Quant x _ y z _ -> extractSubst x <> extractSubst y <> extractSubst z
    Subst before mappings after -> [SUBST (render before) next]
     where
      next :: Map Inlines SUBST
      next =
        Map.fromList [(render mappings, SUBST (render after) substsInAfter)]

      substsInAfter :: Map Inlines SUBST
      substsInAfter = Map.fromList $ map substToEntry (extractSubst after)

      -- entryToSubst :: (Inlines, SUBST) -> SUBST
      -- entryToSubst = snd 

    ArrIdx x y _   -> extractSubst x <> extractSubst y
    ArrUpd x y z _ -> extractSubst x <> extractSubst y <> extractSubst z
    _              -> []


substToEntry :: SUBST -> (Inlines, SUBST)
substToEntry s@(SUBST before _) = (before, s)

--------------------------------------------------------------------------------

--   where
--     run :: String -> FilePath -> TestTree
--     run = runGoldenTest "Server/assets/" $ \sourcePath source -> do
--       return $ serializeTestResult $ runTest sourcePath source $ do
--             program <- parseProgram source
--             Right <$> sweep program


-- specPayloadWithoutIndentationTests :: TestTree
-- specPayloadWithoutIndentationTests =
--   testGroup
--     "Testing specPayloadWithoutIndentation"
--     [ run "mulitline 1" "spec-payload-1.gcl"
--     ]
--   where
--     run :: String -> FilePath -> TestTree
--     run = runGoldenTest "Server/assets/" $ \sourcePath source -> do
--       return $ serializeTestResult $ runTest sourcePath source $ do
--             program <- parseProgram source
--             (_, specs, _, _) <- sweep program
--             return $ Right $ map (map (\x -> "\"" <> x <> "\"") . specPayloadWithoutIndentation source) specs



-- refineSpecsTest :: TestTree
-- refineSpecsTest =
--   testGroup
--     "Refine Specs"
--     [ run "multiline, top-level" "spec-refine-1.gcl"
--     , run "multiline, top-level, indented" "spec-refine-2.gcl"
--     , run "multiline, top-level, poorly indented" "spec-refine-3.gcl"
--     , run "multiline, in IF, 1" "spec-refine-4.gcl"
--     , run "multiline, in IF, 2" "spec-refine-5.gcl"
--     ]
--   where
--     run :: String -> FilePath -> TestTree
--     run = runGoldenTest "Server/assets/" $ \sourcePath source -> do
--       return $ serializeTestResult $ runTest sourcePath source $ do
--             program <- parseProgram source
--             (_, specs, _, _) <- sweep program
--             case listToMaybe specs of
--               Just spec -> do
--                 let range = rangeOf spec
--                 resKind <- handleRefine range 
--                 return $ Right resKind
--               Nothing ->
--                 return $ Left [Others "cannot find any specs"]
