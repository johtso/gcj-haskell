module Tests where

import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck


import Main hiding (main)


makeTest :: (Eq a1, Show a1, Show a) => (a -> a1) -> (a, a1) -> TestTree
makeTest f (a, b) = testCase (show a ++ " -> " ++ show b) (assertEqual "" b (f a))

makeTests :: (Eq a1, Show a, Show a1) => TestName -> (a -> a1) -> [(a, a1)] -> TestTree
makeTests name f caseData = testGroup name $ map (makeTest f) caseData

pathStatusTests :: TestTree
pathStatusTests = makeTests "pathStatus" pathStatus [
    ([Piece X],                   Win X),
    ([Piece O],                   Win O),
    ([T],                         Draw),
    ([Empty],                     Unfinished),
    ([Empty, Empty, Empty],       Unfinished),
    ([Empty, Piece X, Empty],     Unfinished),
    ([Piece X, Piece O, Empty],   Unfinished),
    ([Piece X, Piece X, Piece X], Win X),
    ([Piece X, T, Piece X],       Win X),
    ([T, T, Piece X],             Win X),
    ([T, T, T],                   Draw),
    ([Piece X, Piece X, Piece O], Draw)
    ]

finalGameStatusTests :: TestTree
finalGameStatusTests = makeTests "finalGameStatus" finalGameStatus [
    ([Win X, Unfinished],      Win X),
    ([Win X, Win O],           Win X),
    ([Unfinished, Unfinished], Unfinished),
    ([Unfinished, Draw],       Unfinished),
    ([Win X, Draw],            Win X)
    ]

allTests :: TestTree
allTests = testGroup "Tests" [pathStatusTests, finalGameStatusTests]

main :: IO ()
main = defaultMain allTests
