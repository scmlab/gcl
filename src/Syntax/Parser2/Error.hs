module Syntax.Parser2.Error where
import           Data.List.NonEmpty             ( NonEmpty )
import           Data.Loc                       ( Loc
                                                , Pos
                                                )


--------------------------------------------------------------------------------
-- | Error 

data ParseError = LexicalError Pos
                | SyntacticError (NonEmpty (Loc, String))
                deriving (Eq, Show)
