module GCL.Predicate.Util where

import GCL.Predicate
import GCL.Predicate.Located ()
import Syntax.Abstract (Expr)
import qualified Syntax.Abstract.Operator as A
import Data.Loc (Pos(..), Loc (..), Located (locOf), posLine, unLoc, posCoff)
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Char as Char

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

-- | Smart constructors for testing
pos :: Int -> Int -> Int -> Pos
pos = Pos "<test>"

assertion :: Expr -> Pred
assertion x = Assertion x NoLoc

loopInvariant :: Expr -> Text -> Pred
loopInvariant x b = LoopInvariant x (bnd b) NoLoc
  where
    bnd
      | Text.null b = A.variable
      | Char.isUpper (Text.head b) = A.constant
      | otherwise = A.variable

guardIf :: Expr -> Pred
guardIf x = GuardIf x NoLoc

guardLoop :: Expr -> Pred
guardLoop x = GuardLoop x NoLoc

boundEq :: Expr -> Expr -> Pred
boundEq x var = Bound (x `A.eqq` var) NoLoc

boundLT :: Expr -> Expr -> Pred
boundLT x var = Bound (x `A.lt` var) NoLoc

boundGTE :: Expr -> Expr -> Pred
boundGTE x var = Bound (x `A.gte` var) NoLoc

(===) :: Int -> Int -> Expr
x === y = A.number x `A.eqq` A.number y

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

-- | Return lines within a Spec without indentation
specPayload :: Text -> Spec -> [Text]
specPayload source spec = case specLoc spec of
  NoLoc -> mempty
  Loc start end ->
    let payload = Text.drop (posCoff start) $ Text.take (posCoff end) source
     in init $ tail $ Text.lines payload

-- | Return lines within a Spec without indentation
specPayloadWithoutIndentation :: Text -> Spec -> [Text]
specPayloadWithoutIndentation source spec = 
  let linesWithIndentation = specPayload source spec
      splittedIndentedLines = map (Text.break (not . Char.isSpace)) linesWithIndentation
      smallestIndentation = minimum $ map (Text.length . fst) splittedIndentedLines
      trimmedLines = map (\(indentation, content) -> Text.drop smallestIndentation indentation <> content) splittedIndentedLines
  in trimmedLines