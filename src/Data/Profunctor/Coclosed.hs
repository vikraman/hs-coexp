module Data.Profunctor.Coclosed where

import Control.Monad.Control
import Data.Profunctor

class (Profunctor p) => Coclosed p where
  coclosed :: p a b -> p ((a - c) k) ((b - c) k)
