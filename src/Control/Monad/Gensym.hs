{-# LANGUAGE KindSignatures, MultiParamTypeClasses,
      FunctionalDependencies, FlexibleInstances, FlexibleContexts #-}

{- Revised from
https://hackage.haskell.org/package/hexpr-0.0.0.0
-}
{-| Computations involving the generation of fresh symbols.

    The notion of what is a symbol is abstracted by the 'Gensym' class.
    Then, we provide the 'SymbolGen' monad and 'SymbolGenT' monad transformer,
    in which symbols may be generated.

    Symbols are generated deterministically, but also without reference to any
    other sources of symbols, such as the programmer's algorithms, user input or
    other SymbolGen monads. Therefore, make sure the symbols you generate are
    trivially distinct from all other sources of symbols.
-}
module Control.Monad.Gensym (
    -- * Generate Symbols
      Gensym(..)
  --  , gensym
    -- * Symbol Generator Monad
    , SymbolGen
    , runSymbolGen
    -- * Symbol Generator Monad Transformer
    , SymbolGenT
    , runSymbolGenT
    -- * addition by scm
    , MonadSymGen(..)
    ) where

-- import Control.Applicative
import Control.Monad
import Control.Monad.Identity
import Control.Monad.State.Strict
-- import Control.Monad.Trans
-- import Control.Monad.Trans.Either
-- import Data.Ref (new, newLifted)
-- import qualified Data.Ref as Ref


------ Concepts ------
{-| Class for types that can provide an infinite supply of distinct values. -}
class Gensym s where
    {-| The initial symbol generated. -}
    genzero :: s
    {-| Given the last symbol generated, generate the next.
        Must be distinct from all other symbols generated.
    -}
    nextsym :: s -> s


{-| Monad transformer adding the capability of generating fresh symbols. -}
newtype SymbolGenT s m a = SymbolGenT { unSymbolGenT :: StateT s m a }

{-| Perform a computation involving generating fresh symbols. -}
runSymbolGenT :: (Gensym s, Monad m) => SymbolGenT s m a -> m a
runSymbolGenT = flip evalStateT genzero . unSymbolGenT

{-| Synonym for SymbolGenT over Identity. -}
type SymbolGen s = SymbolGenT s Identity

{-| Synonym for @'runIdentity' . 'runSymbolGenT'@. -}
runSymbolGen :: (Gensym s) => SymbolGen s a -> a
runSymbolGen = runIdentity . runSymbolGenT

------- Basic Instances ------
instance Gensym Integer where
    genzero = 0
    nextsym = (+1)

instance (Monad m) => Functor (SymbolGenT s m) where
    fmap = liftM

instance (Monad m) => Applicative (SymbolGenT s m) where
    pure = return
    (<*>) = ap

instance (Monad m) => Monad (SymbolGenT s m) where
    return = SymbolGenT . return
    x >>= k = SymbolGenT $ unSymbolGenT x >>= unSymbolGenT . k

instance MonadTrans (SymbolGenT s) where
    lift = SymbolGenT . lift

instance (MonadIO m) => MonadIO (SymbolGenT s m) where
    liftIO = lift . liftIO

-- instance (Ref.C m) => Ref.C (SymbolGenT s m) where new = newLifted

------ Transformer Instances ------
-- instance (Ref.C m) => Ref.C (EitherT e m) where new = newLifted

-- TODO instances for other stdlib & spinelib monads

class Monad m => MonadSymGen s (m :: * -> *) | m -> s where
  gensym :: m s


{-| Generate a fresh symbol. Of course, this monad does not know
      what other sources of symbols there are, so make sure your 'Gensym'
      instance generates symbols distinct from all others. -}

instance (Gensym s, Monad m) => MonadSymGen s (SymbolGenT s m) where
  gensym = SymbolGenT $ do
    sym <- get
    modify nextsym
    return sym

-- gensym :: (Gensym s, Monad m) => SymbolGenT s m s
-- gensym = SymbolGenT $ do
--   sym <- get
--   modify nextsym
--   return sym
