import Test.HUnit

import Main


makeTest :: (Eq b, Show b) => (a -> b) -> (a, b) -> Test
makeTest f (a, b) = TestCase (assertEqual "" b (f a))

makeTests :: (Eq b, Show b) => (a -> b) -> [(a, b)] -> Test
makeTests f caseData = TestList $ map (makeTest f) caseData

pathStatusTests :: Test
pathStatusTests = makeTests pathStatus [
    ([Empty, Empty, Empty],       Unfinished),
    ([Empty, Piece X, Empty],     Unfinished),
    ([Piece X, Piece X, Piece X], Win X),
    ([Piece X, T, Piece X],       Win X),
    ([T, T, Piece X],             Win X),
    ([T, T, T],                   Draw),
    ([Piece X, Piece X, Piece O], Draw)
    ]

finalGameStatusTests :: Test
finalGameStatusTests = makeTests finalGameStatus [
    ([Win X, Unfinished],      Win X),
    ([Win X, Win O],           Win X),
    ([Unfinished, Unfinished], Unfinished),
    ([Win X, Draw],            Win X)
    ]

allTests :: Test
allTests = TestList [pathStatusTests, finalGameStatusTests]

main :: IO Counts
main = runTestTT allTests
