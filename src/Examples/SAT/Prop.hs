{-# LANGUAGE DerivingStrategies #-}

module Examples.SAT.Prop where

import Data.Map (Map)
import Data.Map qualified as Map
import Test.Tasty.QuickCheck

newtype Env a = Env (Map String a)
  deriving newtype (Eq, Show, Semigroup, Monoid)

isEmptyEnv :: Env a -> Bool
isEmptyEnv (Env m) = Map.null m

lookupEnv :: String -> Env a -> Maybe a
lookupEnv x (Env m) = Map.lookup x m

insertEnv :: String -> a -> Env a -> Env a
insertEnv x b (Env m) = Env $ Map.insert x b m

isSubEnvOf :: Env a -> Env a -> Bool
isSubEnvOf (Env m1) (Env m2) = Map.isSubmapOfBy (\_ _ -> True) m1 m2

isMatchingSubEnvOf :: (Eq a) => Env a -> Env a -> Bool
isMatchingSubEnvOf (Env m1) (Env m2) = Map.isSubmapOf m1 m2

sizeEnv :: Env a -> Int
sizeEnv (Env m) = Map.size m

eval :: Prop -> Env Bool -> Bool
eval (PVar x) env = Just True == lookupEnv x env
eval PZero _ = False
eval POne _ = True
eval (PAnd p1 p2) env = eval p1 env && eval p2 env
eval (POr p1 p2) env = eval p1 env || eval p2 env
eval (PNot p) env = not $ eval p env

data Prop
  = PVar String
  | PZero
  | POne
  | PAnd Prop Prop
  | POr Prop Prop
  | PNot Prop
  deriving (Eq, Show)

instance Arbitrary Prop where
  arbitrary = sized prop
    where
      arbName = elements $ (: []) <$> ['a' .. 'z']
      prop 0 =
        oneof
          [ PVar <$> arbName,
            pure PZero,
            pure POne
          ]
      prop n =
        oneof
          [ PVar <$> arbName,
            pure PZero,
            pure POne,
            PAnd <$> prop (n - 1) <*> prop (n - 1),
            POr <$> prop (n - 1) <*> prop (n - 1),
            PNot <$> prop (n - 1)
          ]
