{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE TypeOperators #-}

module Control.Monad.Control
  ( type (+),
    type (-),
    type (~>),
    MonadControl (..),
    colam,
    coapp,
  )
where

infixr 5 +

type a + b = Either a b

infixr 5 -

type (b - a) k = k a b

infixr 3 ~>

type (a ~> b) m = a -> m b

class (Monad m) => MonadControl m k where
  cmap :: (c ~> d) m -> ((c - a) k ~> (d - a) k) m
  cocurry :: (c ~> b + a) m -> ((c - a) k ~> b) m
  councurry :: ((c - a) k ~> b) m -> (c ~> b + a) m

  coeval :: (b ~> (b - a) k + a) m
  coeval = councurry pure

  couneval :: (((b + a) - a) k ~> b) m
  couneval = cocurry pure

colam :: (MonadControl m k) => (((() - a) k ~> b) m ~> b + a) m
colam = flip councurry ()

coapp :: (MonadControl m k) => b + a -> ((() - a) k ~> b) m
coapp = cocurry . const . pure
