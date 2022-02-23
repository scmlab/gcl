module Pretty.Variadic where

import           Prettyprinter
import           Control.Monad                  ( (>=>) )

data Variadic a b = Expect (a -> Variadic a b) | Complete b

instance Functor (Variadic a) where
  fmap f (Complete x) = Complete (f x)
  fmap f (Expect   g) = Expect (fmap f . g)

instance Applicative (Variadic a) where
  pure = Complete
  Complete f <*> Complete x = Complete (f x)
  Expect   f <*> Complete x = Expect (\arg -> f arg <*> pure x)
  Complete f <*> Expect   g = Expect (fmap f . g)
  Expect   f <*> Expect   g = Expect (\arg -> f arg <*> g arg)

instance Monad (Variadic a) where
  return = Complete
  Complete x >>= f = f x
  Expect   g >>= f = Expect (g >=> f)

var :: Variadic a a
var = Expect Complete

parensIf :: Int -> Int -> Doc ann -> Doc ann
parensIf n m | n > m     = parens
             | otherwise = id

