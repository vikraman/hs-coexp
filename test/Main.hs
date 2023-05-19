module Main (main) where

import Data.Maybe
import Examples.SAT.Backtrack qualified as B
import Examples.SAT.Guess qualified as G
import Examples.SAT.Prop
import Test.Tasty
import Test.Tasty.QuickCheck

main :: IO ()
main =
  defaultMain $
    testGroup
      "coexp"
      [ testGroup
          "Examples.SAT"
          [ testGroup
              "bruteforce SAT solver"
              [ testProperty "produces a satisfying assignment" $
                  forAll arbitrary $ \(phi :: Prop) ->
                    let env = G.sat phi
                     in not (isEmptyEnv env) ==> eval phi env
              ],
            testGroup
              "backtracking SAT solver"
              [ testProperty "produces a satisfying assignment" $
                  forAll arbitrary $ \(phi :: Prop) ->
                    let maybeEnv = B.sat phi
                     in eval phi <$> maybeEnv
              ],
            testProperty "backtracking produces a shorter assignment" $
              forAll arbitrary $ \(phi :: Prop) ->
                let genv = G.sat phi
                    benv = fromMaybe mempty (B.sat phi)
                 in benv `isSubEnvOf` genv,
            testProperty "both assignments match up" $
              forAll arbitrary $ \(phi :: Prop) ->
                let genv = G.sat phi
                    benv = fromMaybe mempty (B.sat phi)
                 in benv `isMatchingSubEnvOf` genv .||. eval phi benv == eval phi genv
          ]
      ]
