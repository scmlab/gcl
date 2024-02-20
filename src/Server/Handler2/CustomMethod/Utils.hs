{-# LANGUAGE OverloadedStrings #-}
module Server.Handler2.CustomMethod.Utils where
import Server.Monad (LoadedProgram(..), ServerM)
import qualified Syntax.Abstract as A
import Data.Loc.Range (Range, fromLoc, withinRange)
import Syntax.Abstract (Expr)
import GCL.Predicate (PO (..), Spec (..))
import GCL.WP.Type (StructWarning)
import Server.CustomMethod (ResKind (..))
import Data.Text (Text)
import Render (Section (..), Render (..), RenderSection (..), Block (..), Deco (..))
import Server.Handler.Diagnostic (Collect(..))
import Server.Handler2.Utils (sendDiagnostics, bumpVersion)
import Data.Maybe (mapMaybe)
import Data.Loc (locOf)
import Pretty (toText)

sendDiagnosticsAndMakeResponseFromLoadedProgram :: FilePath -> LoadedProgram -> Maybe Range -> ServerM [ResKind]
sendDiagnosticsAndMakeResponseFromLoadedProgram filePath loadedProgram rangeToFocus = do
  -- destructure and get needed data
  let (LoadedProgram
          _ _ (A.Program _ _ globalProperties _ _)
          _ _ proofObligations specs warnings _ _
        ) = loadedProgram
  
  -- send warnings as diagnostics
  let diagnostics = concatMap collect warnings
  sendDiagnostics filePath diagnostics

  -- send reponse for client to render hints
  version' <- bumpVersion
  let response = makeResponseFromHints version' rangeToFocus (globalProperties, proofObligations, specs, warnings)
  return response

makeResponseFromHints :: Int -> Maybe Range -> ([Expr], [PO], [Spec], [StructWarning]) -> [ResKind]
makeResponseFromHints version rangeToInspect _hints@(globalProperties, proofObligations, specs, warnings) =
  [ ResMarkPOs rangesOfProofObligations
  , ResUpdateSpecs (map encodeSpec specs)
  , ResDisplay version sections
  ]
  where
    rangesOfProofObligations :: [Range]
    rangesOfProofObligations = mapMaybe (fromLoc . locOf) proofObligations
    encodeSpec :: Spec -> (Int, Text, Text, Range)
    encodeSpec spec = 
      ( specID spec
      , toText $ render (specPreCond spec)
      , toText $ render (specPostCond spec)
      , specRange spec
      )
    sections :: [Section]
    sections = mconcat
      [ warningsSections
      , specsSections
      , proofObligationSections
      , globalPropertiesSections
      ]
      where 
        warningsSections :: [Section]
        warningsSections = map renderSection warnings
        specsSections :: [Section]
        specsSections = map renderSection specsToDisplay
          where
            specsToDisplay :: [Spec]
            specsToDisplay = case rangeToInspect of
              Nothing            -> specs
              Just selectedRange -> filter (withinRange selectedRange) specs 
        proofObligationSections :: [Section]
        proofObligationSections = map renderSection proofObligationsToDisplay
          where
            proofObligationsToDisplay :: [PO]
            proofObligationsToDisplay = case rangeToInspect of
              Nothing        -> proofObligations
              Just selectedRange -> filter (selectedRange `covers`) proofObligations
              where
                covers :: Range -> PO -> Bool
                covers range po = case poAnchorLoc po of
                  Nothing     -> withinRange range po
                  Just anchor -> withinRange range po || withinRange range anchor
        globalPropertiesSections :: [Section]
        globalPropertiesSections = map
                (\expr -> Section
                  Plain
                  [Header "Property" (fromLoc (locOf expr)), Code (render expr)]
                )
                globalProperties
