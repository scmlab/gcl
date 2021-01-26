{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase, FlexibleContexts #-}

module GCL.Exec.ExecMonad where

import           Data.Aeson
import           Data.Loc
import           Control.Monad.Except
import           Control.Monad.State     hiding ( guard )
import           GHC.Generics

import           Syntax.Abstract        

type Store = [(Var, Val)]

data Val = VNum Int | VBol Bool | VChr Char
         | VFun (Val -> Either ExecError Val)
         | VArr Int [Val]
         | Undef

data ExecError = Aborted Loc
               | AllFailedInIf Loc
               | DivByZero Loc
               | ArrayOutOfBound Int Int Loc
 deriving (Show, Eq, Generic)

instance ToJSON ExecError where
instance Located ExecError where
  locOf (Aborted       l      ) = l
  locOf (AllFailedInIf l      ) = l
  locOf (DivByZero     l      ) = l
  locOf (ArrayOutOfBound _ _ l) = l

class (MonadPlus m, MonadError ExecError m, MonadState Store m)
           => ExecMonad m where
  lookupStore :: Loc -> Var -> m Val
  updateStore :: Loc -> Var -> Val -> m ()
  shuffle :: [GdCmd] -> m [GdCmd]

  lookupStore l x =
    (lookup x <$> get) >>= \case
     Nothing -> error "shouldn't happen"
     Just (VArr n xs) -> return (VFun (arrToFun l n xs))
     Just v -> return v

  updateStore _ x v = do
    store <- get
    put ((x,v) : filter (not . (==x) . fst) store)

  shuffle = return

arrToFun :: Loc -> Int -> [a] -> Val -> Either ExecError a
arrToFun l n xs (VNum i) | i < n     = Right (xs !! i)
                         | otherwise = Left (ArrayOutOfBound i n l)
arrToFun _ _ _ _ = error "type error, shouldn't hapen"

instance Show Val where
  showsPrec p (VNum i   ) = showsPrec p i
  showsPrec p (VBol b   ) = showsPrec p b
  showsPrec p (VChr c   ) = showsPrec p c
  showsPrec _ (VFun _   ) = ("<Fun>" ++)
  showsPrec p (VArr _ xs) = showsPrec p xs
  showsPrec _ Undef       = ("undef" ++)
