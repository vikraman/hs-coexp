{-# LANGUAGE DerivingStrategies #-}

module Control.Monad.Control
  ( MonadControl (..),
  )
where

class (Monad m) => MonadControl m k where
  cmap :: (c -> m d) -> k a c -> m (k a d)
  cocurry :: (c -> m (Either b a)) -> k a c -> m b
  councurry :: (k a c -> m b) -> c -> m (Either b a)

  coeval :: b -> m (Either (k a b) a)
  coeval = councurry pure

  couneval :: k a (Either b a) -> m b
  couneval = cocurry pure
