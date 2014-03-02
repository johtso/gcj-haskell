module Tests where

import Test.Framework (defaultMain, testGroup, Test, TestName)
import Test.Framework.Providers.HUnit (testCase)

import Test.HUnit hiding (Test)

import Main hiding (main)


makeTest :: (Eq b, Show a, Show b) => (a -> b) -> (a, b) -> Test
makeTest f (a, b) = testCase (show a ++ " -> " ++ show b) (assertEqual "" b (f a))

makeTests :: (Eq b, Show a, Show b) => TestName -> (a -> b) -> [(a, b)] -> Test
makeTests name f caseData = testGroup name $ map (makeTest f) caseData

pathStatusTests :: Test
pathStatusTests = makeTests "pathStatus" pathStatus [
    ([Empty, Empty, Empty],       Unfinished),
    ([Empty, Piece X, Empty],     Unfinished),
    ([Piece X, Piece O, Empty],   Unfinished),
    ([Piece X, Piece X, Piece X], Win X),
    ([Piece X, T, Piece X],       Win X),
    ([T, T, Piece X],             Win X),
    ([T, T, T],                   Draw),
    ([Piece X, Piece X, Piece O], Draw)
    ]

finalGameStatusTests :: Test
finalGameStatusTests = makeTests "finalGameStatus" finalGameStatus [
    ([Win X, Unfinished],      Win X),
    ([Win X, Win O],           Win X),
    ([Unfinished, Unfinished], Unfinished),
    ([Win X, Draw],            Win X)
    ]

allTests :: [Test]
allTests = [pathStatusTests, finalGameStatusTests]

main :: IO ()
main = defaultMain allTests
