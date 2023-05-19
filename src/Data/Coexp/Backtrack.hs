module Data.Coexp.Backtrack where

import Control.Monad.Cont
import Control.Monad.Control
import Data.Coexp

assumeLeft :: ((a -> r) -> Cont r b) -> Cont r (b + a)
assumeLeft = colamCont

resolveLeft :: (b + a) -> (a -> r) -> Cont r b
resolveLeft = coappCont

swap :: a + b -> b + a
swap = either Right Left

assumeRight :: ((b -> r) -> Cont r a) -> Cont r (b + a)
assumeRight = fmap swap . assumeLeft

resolveRight :: (b + a) -> (b -> r) -> Cont r a
resolveRight = resolveLeft . swap

assumeBoth :: ((a -> r) -> (b -> r) -> r) -> Cont r (b + a)
assumeBoth = assumeLeft . (cont .)

resolveBoth :: Cont r (b + a) -> (b -> r) -> (a -> r) -> r
resolveBoth m k = runCont (m >>= (`resolveRight` k))
