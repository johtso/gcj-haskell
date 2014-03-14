import Data.Char
import Data.List
import Text.Printf

type Pattern = [[Int]]
type Path = [Int]


mowable :: Path -> [Bool]
mowable xs = map (== maximum xs) xs

mowableHorizontally :: Pattern -> [[Bool]]
mowableHorizontally = map mowable

mowableVertically :: Pattern -> [[Bool]]
mowableVertically = transpose . map mowable . transpose

solve :: Pattern -> Bool
solve xss = all (uncurry (||)) . concat $ zipWith zip (mowableHorizontally xss) (mowableVertically xss)

sections :: [String] -> [[String]]
sections = getSections . tail
    where
        getSections [] = []
        getSections (x:xs) = take size xs : getSections (drop size xs)
            where size = digitToInt (head x)

parsePattern :: [String] -> Pattern
parsePattern = map (map (digitToInt . head) . words)

unMarshal :: [String] -> [Pattern]
unMarshal = map parsePattern . sections

caseLine :: Int -> Bool -> String
caseLine n possible = printf "Case #%d: %s" n message
    where message = if possible then "YES" else "NO"

marshal :: [Bool] -> [String]
marshal = zipWith caseLine [1..]

main :: IO ()
main = interact $ unlines . marshal . map solve . unMarshal . lines
