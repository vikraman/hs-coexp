module Data.Functor.Strong
  ( LeftStrong (..),
    RightStrong (..),
    Strong,
  )
where

class (Functor f) => LeftStrong f p where
  leftStrength :: a `p` f b -> f (a `p` b)

instance (Functor f) => LeftStrong f (,) where
  leftStrength = uncurry (fmap . (,))

class (Functor f) => RightStrong f p where
  rightStrength :: f a `p` b -> f (a `p` b)

instance (Functor f) => RightStrong f (,) where
  rightStrength = uncurry ((. flip (,)) . flip fmap)

class (Functor f, LeftStrong f p, RightStrong f p) => Strong f p
