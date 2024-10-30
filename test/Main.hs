module Main (main) where

import Data.Maybe
import Examples.Eff.Toss qualified as T
import Examples.SAT.Backtrack qualified as B
import Examples.SAT.Guess qualified as G
import Examples.SAT.Prop
import Examples.TS.Backtrack qualified as B
import Examples.TS.Tree
import Test.Tasty
import Test.Tasty.HUnit
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
          ],
        testGroup
          "Examples.Eff"
          [ testGroup
              "Toss"
              [ testCase "handle pure with Maybe" $
                  T.handleOpWithMaybe (pure (42 :: Int)) @?= Just 42,
                testCase "handle fail with Maybe" $
                  T.handleOpWithMaybe (T.fail @Int) @?= Nothing,
                testCase "handle pure with List" $
                  T.handleOpWithList (pure (42 :: Int)) @?= [42],
                testCase "handle toss with List" $
                  T.handleOpWithList T.toss @?= [T.Head, T.Tail],
                testCase "handle drunkToss with List" $
                  T.handleOpWithList T.drunkToss @?= [T.Head, T.Tail],
                testCase "handle drunkTosses 2 with List" $
                  T.handleOpWithList (T.drunkTosses 2) @?= [[T.Head, T.Head], [T.Head, T.Tail], [T.Tail, T.Head], [T.Tail, T.Tail]],
                testCase "handle drunkTosses 2 with Maybe then List" $
                  T.handleOpWithMaybeThenList (T.drunkTosses 2) @?= [Just [T.Head, T.Head], Just [T.Head, T.Tail], Nothing, Just [T.Tail, T.Head], Just [T.Tail, T.Tail], Nothing, Nothing],
                testCase "handle drunkTosses 2 with List then Maybe" $
                  T.handleOpWithListThenMaybe (T.drunkTosses 2) @?= Just [[T.Head, T.Head]]
              ]
          ],
        testGroup
          "Examples.TS"
          [ testGroup
              "backtracking tree search"
              [ testCase "finds a shortest path" $
                  let tree1 :: Tree Int =
                        Node
                          (Node (Node Leaf 2 Leaf) 3 (Node Leaf 4 Leaf))
                          5
                          (Node (Node Leaf 6 Leaf) 7 (Node Leaf 8 Leaf))
                   in B.find tree1 (== 6) @?= Just [6, 7, 5]
              ]
          ]
      ]
