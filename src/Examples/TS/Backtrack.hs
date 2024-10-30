{-# LANGUAGE LambdaCase #-}

module Examples.TS.Backtrack where

import Control.Monad.Cont
import Control.Monad.Control
import Data.Coexp.Backtrack
import Examples.TS.Tree
import Prelude hiding (fail, succ)

type Succ a = [a]

type Fail = ()

search :: [a] -> Tree a -> (a -> Bool) -> Cont r (Fail + Succ a)
search path t p =
  case t of
    Leaf ->
      assumeLeft $ \_ -> return ()
    Node l a r ->
      if p a
        then assumeLeft $ \succ ->
          resolveLeft
            (return (a : path))
            succ
        else assumeBoth $ \succ fail ->
          resolveBoth
            (search (a : path) l p)
            (\() -> resolveBoth (search (a : path) r p) fail succ)
            succ

find :: Tree a -> (a -> Bool) -> Maybe [a]
find t p =
  runCont (search [] t p) $ \case
    Left () -> Nothing
    Right path -> if not (null path) then Just path else Nothing
