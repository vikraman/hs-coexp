{-# LANGUAGE DerivingStrategies #-}

module Examples.Eff.Toss where

import Control.Applicative
import Control.Monad hiding (fail)
import Control.Monad.Free.Control
import Data.Functor.Sum
import Data.Void
import Prelude hiding (fail)

newtype FailF r = Fail (Void -> r) deriving stock (Functor)

newtype ChooseF r = Choose (Bool -> r) deriving stock (Functor)

type OpF = Sum FailF ChooseF

type Fail r = Free FailF r

type Choose r = Free ChooseF r

type Op r = Free OpF r

data Toss = Head | Tail deriving (Eq, Show)

fail :: Op a
fail = wrap $ InL $ Fail absurd

toss :: Op Toss
toss = wrap $
  InR $
    Choose $ \b ->
      if b then pure Head else pure Tail

drunkToss :: Op Toss
drunkToss = wrap $
  InR $
    Choose $ \b ->
      if b
        then toss
        else fail

drunkTosses :: Int -> Op [Toss]
drunkTosses n =
  if n == 0
    then pure []
    else do
      t <- drunkToss
      ts <- drunkTosses (n - 1)
      pure (t : ts)

nabla :: (f r -> r) -> (g r -> r) -> (Sum f g r -> r)
nabla algF _ (InL f) = algF f
nabla _ algG (InR g) = algG g

handleFailWithMaybe :: Fail a -> Maybe a
handleFailWithMaybe = foldFree alg gen
  where
    alg :: FailF (Maybe a) -> Maybe a
    alg (Fail _) = Nothing
    gen :: a -> Maybe a
    gen = Just

handleFailAndGWithMaybe :: (Functor g) => Free (Sum FailF g) a -> Free g (Maybe a)
handleFailAndGWithMaybe = foldFree (nabla alg wrap) gen
  where
    alg :: (Functor g) => FailF (Free g (Maybe a)) -> Free g (Maybe a)
    alg (Fail _) = pure Nothing
    gen :: (Functor g) => a -> Free g (Maybe a)
    gen = pure . Just

handleFailAndGWithList :: (Functor g) => Free (Sum FailF g) a -> Free g [a]
handleFailAndGWithList = foldFree (nabla alg wrap) gen
  where
    alg :: (Functor g) => FailF (Free g [a]) -> Free g [a]
    alg (Fail _) = pure []
    gen :: (Functor g) => a -> Free g [a]
    gen = pure . return

handleChooseWithMaybe :: Choose a -> Maybe a
handleChooseWithMaybe = foldFree alg gen
  where
    alg :: ChooseF (Maybe a) -> Maybe a
    alg (Choose k) = k True <|> k False
    gen :: a -> Maybe a
    gen = Just

handleChooseAndGWithMaybe :: (Functor g) => Free (Sum ChooseF g) a -> Free g (Maybe a)
handleChooseAndGWithMaybe = foldFree (nabla alg wrap) gen
  where
    alg :: (Functor g) => ChooseF (Free g (Maybe a)) -> Free g (Maybe a)
    alg (Choose k) = k True >>= \mx -> k False >>= \my -> pure (mx <|> my)
    gen :: (Functor g) => a -> Free g (Maybe a)
    gen = pure . Just

handleChooseWithList :: Choose a -> [a]
handleChooseWithList = foldFree alg gen
  where
    alg :: ChooseF [a] -> [a]
    alg (Choose k) = k True ++ k False
    gen :: a -> [a]
    gen = return

handleChooseAndGWithList :: (Functor g) => Free (Sum ChooseF g) a -> Free g [a]
handleChooseAndGWithList = foldFree (nabla alg wrap) gen
  where
    alg :: (Functor g) => ChooseF (Free g [a]) -> Free g [a]
    alg (Choose k) = k True >>= \xs -> k False >>= \ys -> pure (xs ++ ys)
    gen :: (Functor g) => a -> Free g [a]
    gen = pure . return

handleOpWithMaybe :: Op a -> Maybe a
handleOpWithMaybe = join . handleChooseWithMaybe . handleFailAndGWithMaybe

handleOpWithList :: Op a -> [a]
handleOpWithList = join . handleChooseWithList . handleFailAndGWithList

handleOpWithMaybeThenList :: Op a -> [Maybe a]
handleOpWithMaybeThenList = handleChooseWithList . handleFailAndGWithMaybe

handleOpWithListThenMaybe :: Op a -> Maybe [a]
handleOpWithListThenMaybe = handleChooseWithMaybe . handleFailAndGWithList
