{-# LANGUAGE LambdaCase #-}

module Examples.SAT.Backtrack where

import Control.Monad.Cont
import Control.Monad.Control
import Data.Coexp.Backtrack
import Examples.SAT.Prop
import Prelude hiding (fail, succ)

type Succ r = (Env Bool, Bool, () -> r)

type Fail = ()

solve :: Env Bool -> Prop -> Cont r (Fail + Succ r)
solve _env phi =
  case phi of
    PVar x ->
      case lookupEnv x _env of
        Just b ->
          assumeRight $ \fail ->
            return (_env, b, fail)
        Nothing ->
          assumeBoth $ \succ fail ->
            succ (insertEnv x True _env, True, \() -> succ (insertEnv x False _env, False, fail))
    PZero ->
      assumeLeft $ \_ ->
        return ()
    POne ->
      assumeRight $ \fail ->
        return (_env, True, fail)
    PAnd p1 p2 ->
      assumeLeft $ \succ -> do
        s1 <- solve _env p1
        resolveLeft s1 $ \(_env, b1, fail1) ->
          if b1
            then resolveBoth (solve _env p2) fail1 $ \(_env, b2, fail2) ->
              succ (_env, b1 && b2, fail2)
            else fail1 ()
    POr p1 p2 ->
      assumeLeft $ \succ -> do
        s1 <- solve _env p1
        resolveLeft s1 $ \(_env, b1, fail1) ->
          if b1
            then succ (_env, True, fail1)
            else resolveBoth (solve _env p2) fail1 $ \(_env, b2, fail2) ->
              succ (_env, b1 || b2, fail2)
    PNot p ->
      assumeLeft $ \succ -> do
        s <- solve _env p
        resolveLeft s $ \(_env, b, fail) ->
          succ (_env, not b, fail)

sat :: Prop -> Maybe (Env Bool)
sat phi =
  runCont (solve mempty phi) $ \case
    Left () -> Nothing
    Right (env, b, fail) -> if b then Just env else fail ()
