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

runCoexp :: Coexp r a a -> r
runCoexp = eval . swap . second unKont . unCoexp

cmapCont :: (a -> Cont r b) -> Coexp r c a -> Cont r (Coexp r c b)
cmapCont f = leftStrength . fmap f

counevalCont :: Coexp r a (Either b a) -> Cont r b
counevalCont = either pure (cont . const . runCoexp) . leftStrength

cocurryCont :: (c -> Cont r (Either b a)) -> Coexp r a c -> Cont r b
cocurryCont f = counevalCont <=< cmapCont f

split :: (Either a b -> c) -> (a -> c, b -> c)
split f = (f . Left, f . Right)

coevalCont :: b -> Cont r (Either (Coexp r a b) a)
coevalCont b = cont (eval . second (Coexp . (b,) . Kont) . split)

councurryCont :: (Coexp r a c -> Cont r b) -> c -> Cont r (Either b a)
councurryCont f = either (fmap Left . f) (pure . Right) <=< coevalCont

instance MonadControl (Cont r) (Coexp r) where
  cmap = cmapCont
  cocurry = cocurryCont
  councurry = councurryCont
  coeval = coevalCont
  couneval = counevalCont
