module GCL.Predicate.Util where

import qualified Data.Char as Char
import Data.Loc (Loc (..), Located (locOf), posCoff, posLine, unLoc)
import Data.Text (Text)
import qualified Data.Text as Text
import GCL.Predicate
import GCL.Predicate.Located ()
import Syntax.Abstract (Expr)
import qualified Syntax.Abstract.Operator as A
import Data.Loc.Range (Range(Range))
{-
toExpr :: Pred -> Expr
toExpr (Constant e) = e
toExpr (Bound e _) = e
toExpr (Assertion e _) = e
toExpr (LoopInvariant e _ _) = e
toExpr (GuardIf e _) = e
toExpr (GuardLoop e _) = e
toExpr (Conjunct xs) = A.conjunct (map toExpr xs)
toExpr (Disjunct xs) = A.disjunct (map toExpr xs)
toExpr (Negate x) = A.neg (toExpr x)

guardIf :: Expr -> Pred
guardIf x = GuardIf x (locOf x)

guardLoop :: Expr -> Pred
guardLoop x = GuardLoop x (locOf x)

conjunct :: [Pred] -> Pred
conjunct [] = Constant A.true
conjunct [x] = x
conjunct xs = Conjunct xs

disjunct :: [Pred] -> Pred
disjunct [] = Constant A.false
disjunct [x] = x
disjunct xs = Disjunct xs

-- extracting the assertion from a Struct
extractAssertion :: Struct -> Pred
extractAssertion (Struct p _ _) = p
extractAssertion (Postcond p) = p

-- given a line number, get the precondition of the statement after that line
precondAtLine :: Int -> Struct -> Maybe Pred
precondAtLine i = findNext i . toPredList
  where
    toPredList :: Struct -> [Pred]
    toPredList (Postcond p) = [p]
    toPredList (Struct p xs next) = p : map precond xs ++ toPredList next

    findNext :: Int -> [Pred] -> Maybe Pred
    findNext _ [] = Nothing
    findNext n (p : ps) = case locOf p of
      NoLoc -> Nothing
      Loc start _ -> if n <= posLine start then Just p else findNext n ps

precond :: Stmt -> Pred
precond (Skip l) = unLoc l
precond (Abort l) = unLoc l
precond (Assign l _ _) = unLoc l
precond (Do l _ _) = unLoc l
precond (If l _) = unLoc l
precond (Spec l _) = unLoc l
-}
-- | Return lines within a Spec without indentation
specPayloadWithoutIndentation :: Text -> Spec -> [Text]
specPayloadWithoutIndentation source spec =
  let Range start end = specRange spec
      spansMultipleLines = posLine start /= posLine end
    in if spansMultipleLines
      then
        let payload = Text.drop (posCoff start) $ Text.take (posCoff end) source
            linesWithIndentation = init $ tail $ Text.lines payload
            splittedIndentedLines = map (Text.break (not . Char.isSpace)) linesWithIndentation
            smallestIndentation = minimum $ map (Text.length . fst) splittedIndentedLines
            trimmedLines = map (\(indentation, content) -> Text.drop smallestIndentation indentation <> content) splittedIndentedLines
        in trimmedLines
      else
        [Text.strip $ Text.drop (posCoff start + 2) $ Text.take (posCoff end - 2) source]
