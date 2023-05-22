module Control.Arrow.Coapply
  ( ArrowCoapply (..),
  )
where

import Control.Arrow
import Control.Monad.Control

class (Arrow a) => ArrowCoapply a k where
  coapp :: a c ((c - b) k + b)

instance (MonadControl m k) => ArrowCoapply (Kleisli m) k where
  coapp = Kleisli coeval
