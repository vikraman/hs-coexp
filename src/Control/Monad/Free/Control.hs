{-# LANGUAGE DerivingStrategies #-}

module Control.Monad.Free.Control
  ( Free (..),
    MonadFree (..),
    foldFree,
  )
where

import Control.Monad.Cont
import Control.Monad.Control
import Control.Monad.Free (MonadFree (..))
import Data.Coexp

newtype Free f a = Free {runFree :: forall r. (f r -> r) -> Cont r a}
  deriving stock (Functor)

colamFree :: (Free f a ~> a + f r) (Cont r)
colamFree f = colamCont $ \alg -> cont $ \gen -> runCont (runFree f alg) gen

instance Applicative (Free f) where
  pure a = Free $ \_ -> pure a
  Free f <*> Free g = Free $ \alg -> f alg <*> g alg

instance Monad (Free f) where
  return = pure
  Free f >>= g = Free $ \alg -> f alg >>= \a -> runFree (g a) alg

reset0 :: Cont r r -> r
reset0 = flip runCont id

foldFree :: (Functor f) => (f r -> r) -> (a -> r) -> Free f a -> r
foldFree alg gen = reset0 . fmap (either gen alg) . colamFree

instance (Functor f) => MonadFree f (Free f) where
  wrap f = Free $ \alg -> cont $ \gen ->
    alg (fmap (foldFree alg gen) f)
