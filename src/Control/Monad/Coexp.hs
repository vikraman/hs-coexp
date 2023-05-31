{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE TypeOperators #-}

module Control.Monad.Coexp
  ( type (+),
    type (-),
    type (#),
    type (~>),
    MonadCoexp (..),
    lem,
    callCC,
    throw,
  )
where

infixr 5 +

type a + b = Either a b

infixr 5 -

type (b - a) k = k a b

infix 7 #

type a # k = (() - a) k

infixr 3 ~>

type (a ~> b) m = a -> m b

class (Monad m) => MonadCoexp m k where
  cmap :: (c ~> d) m -> ((c - a) k ~> (d - a) k) m
  cocurry :: (c ~> b + a) m -> ((c - a) k ~> b) m
  councurry :: ((c - a) k ~> b) m -> (c ~> b + a) m

  coeval :: (b ~> (b - a) k + a) m
  coeval = councurry pure

  couneval :: (((b + a) - a) k ~> b) m
  couneval = cocurry pure

  colam :: (MonadCoexp m k) => ((a # k ~> b) m ~> b + a) m
  colam = flip councurry ()

  coapp :: (MonadCoexp m k) => ((b + a, a # k) ~> b) m
  coapp = uncurry (cocurry . const . pure)

lem :: (MonadCoexp m k) => (() ~> a # k + a) m
lem () = colam pure

codiag :: a + a -> a
codiag = either id id

callCC :: (MonadCoexp m k) => ((a # k ~> a) m ~> a) m
callCC = fmap codiag . colam

throw :: (MonadCoexp m k) => ((a - a) k ~> b) m
throw = cocurry (pure . Right)
