module Examples.SAT.Guess where

import Control.Monad
import Control.Monad.Cont
import Examples.SAT.Prop

shift :: ((a -> r) -> Cont r r) -> Cont r a
shift f = cont (reset . f)

reset :: Cont r r -> r
reset = flip runCont id

oneof :: (Semigroup r) => a -> a -> Cont r a
oneof a b = shift $ \k ->
  return (k a <> k b)

anyof :: (Semigroup r) => a -> [a] -> Cont r a
anyof d [] =
  return d
anyof d (x : xs) =
  anyof d xs >>= oneof x

fv :: Prop -> Maybe String
fv (PVar x) = Just x
fv PZero = Nothing
fv POne = Nothing
fv (PAnd p1 p2) = fv p1 `mplus` fv p2
fv (POr p1 p2) = fv p1 `mplus` fv p2
fv (PNot p) = fv p

guess :: String -> Bool -> Prop -> Prop
guess x b phi =
  case phi of
    PVar y -> if x == y then if b then POne else PZero else phi
    PZero -> PZero
    POne -> POne
    PAnd p1 p2 -> PAnd (guess x b p1) (guess x b p2)
    POr p1 p2 -> POr (guess x b p1) (guess x b p2)
    PNot p -> PNot (guess x b p)

guessOne :: Env Bool -> Prop -> Cont (Env Bool) (Env Bool)
guessOne env phi =
  case fv phi of
    Nothing ->
      return env
    Just x -> do
      b <- oneof True False
      guessOne (insertEnv x b env) (guess x b phi)

sat :: Prop -> Env Bool
sat phi =
  reset $ do
    env <- guessOne mempty phi
    if eval phi env
      then return env
      else return mempty
