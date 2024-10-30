module Examples.TS.Tree where

data Tree a
  = Leaf
  | Node (Tree a) a (Tree a)
  deriving (Eq, Show)
