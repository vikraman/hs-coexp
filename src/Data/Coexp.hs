{-# LANGUAGE DerivingStrategies #-}

module Data.Coexp
  ( Coexp (..),
  )
where

import Control.Monad
import Control.Monad.Cont
import Control.Monad.Control
import Data.Bifunctor
import Data.Functor.Contravariant
import Data.Functor.Strong
import Data.Profunctor
import Data.Tuple

newtype Kont r a = Kont {unKont :: a -> r}

instance Contravariant (Kont r) where
  contramap f = Kont . (. f) . unKont

newtype Coexp r a b = Coexp {unCoexp :: (b, Kont r a)}
  deriving stock (Functor)

instance Profunctor (Coexp r) where
  dimap f g = Coexp . bimap g (contramap f) . unCoexp

instance (Functor f) => LeftStrong f (Coexp r) where
  leftStrength = fmap Coexp . rightStrength . unCoexp

eval :: (a -> b, a) -> b
eval = uncurry id

runCoexp :: (a - a) (Coexp r) -> r
runCoexp = eval . swap . second unKont . unCoexp

cmapCont :: (a ~> b) (Cont r) -> ((a - c) (Coexp r) ~> (b - c) (Coexp r)) (Cont r)
cmapCont f = leftStrength . fmap f

counevalCont :: (((b + a) - a) (Coexp r) ~> b) (Cont r)
counevalCont = either pure (cont . const . runCoexp) . leftStrength

cocurryCont :: (c ~> b + a) (Cont r) -> ((c - a) (Coexp r) ~> b) (Cont r)
cocurryCont f = counevalCont <=< cmapCont f

split :: (a + b -> c) -> (a -> c, b -> c)
split f = (f . Left, f . Right)

coevalCont :: (b ~> (b - a) (Coexp r) + a) (Cont r)
coevalCont b = cont (eval . second (Coexp . (b,) . Kont) . split)

councurryCont :: ((c - a) (Coexp r) ~> b) (Cont r) -> (c ~> b + a) (Cont r)
councurryCont f = either (fmap Left . f) (pure . Right) <=< coevalCont

instance MonadControl (Cont r) (Coexp r) where
  cmap = cmapCont
  cocurry = cocurryCont
  councurry = councurryCont
  coeval = coevalCont
  couneval = counevalCont
