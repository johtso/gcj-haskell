module Tests where

import Data.List (nub, sort)
import Data.Maybe (fromMaybe, listToMaybe)

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
    ([],                          Draw),
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
    ([],                       Draw),
    ([Win X, Unfinished],      Win X),
    ([Win X, Win O],           Win X),
    ([Unfinished, Unfinished], Unfinished),
    ([Unfinished, Draw],       Unfinished),
    ([Win X, Draw],            Win X)
    ]

instance Arbitrary Square where
    arbitrary = elements [Piece X, Piece O, T, Empty]

instance Arbitrary GameStatus where
    arbitrary = elements [Win X, Win O, Unfinished, Draw]

prop_pathStatus :: [Square] -> Bool
prop_pathStatus xs = pathStatus xs == expectedResult
    where expectedResult
           | Empty `elem` essence = Unfinished
           | otherwise = essenceToResult essence
                where
                    essence = sort (nub xs)
                    essenceToResult [Piece a] = Win a
                    essenceToResult [Piece a, T] = Win a
                    essenceToResult _ = Draw


isWin :: GameStatus -> Bool
isWin (Win _) = True
isWin _ = False

prop_finalGameStatus :: [GameStatus] -> Bool
prop_finalGameStatus xs = finalGameStatus xs == expectedResult
    where expectedResult
           | null xs = Draw
           | otherwise = fromMaybe nonWin maybeWins
                where
                    maybeWins = listToMaybe (filter isWin xs)
                    nonWin = if Unfinished `elem` xs
                             then Unfinished
                             else Draw

allTests :: TestTree
allTests = testGroup "Tests" [
    testProperty "pathStatus" prop_pathStatus,
    testProperty "finalGameStatus" prop_finalGameStatus,
    pathStatusTests,
    finalGameStatusTests
    ]

main :: IO ()
main = defaultMain allTests
