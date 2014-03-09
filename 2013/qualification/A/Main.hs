import Data.List
import Data.List.Split
import Data.Maybe
import Text.Printf


data Player = X | O deriving (Eq, Ord, Show)
data Square = Piece Player | T | Empty deriving (Eq, Ord, Show)
type Board = [[Square]]
data GameStatus = Win Player | Draw | Unfinished deriving (Eq, Ord, Show)


enumerateFrom :: Int -> [a] -> [(Int, a)]
enumerateFrom n = zip [n..]

enumerate :: [a] -> [(Int, a)]
enumerate = enumerateFrom 0

mapApply :: [a -> b] -> a -> [b]
mapApply fs x = map ($x) fs

concatMapApply :: [a -> [b]] -> a -> [b]
concatMapApply fs x = concat $ mapApply fs x

diagonals :: [[a]] -> [[a]]
diagonals xss = [mainDiagonal xss, minorDiagonal xss]

mainDiagonal :: [[a]] -> [a]
mainDiagonal xss = [xs!!n | (n, xs) <- enumerate xss]

minorDiagonal :: [[a]] -> [a]
minorDiagonal xss = mainDiagonal (map reverse xss)

pathsThroughGrid :: [[a]] -> [[a]]
pathsThroughGrid = concatMapApply [id, transpose, diagonals]

charToSquare :: Char -> Square
charToSquare 'X' = Piece X
charToSquare 'O' = Piece O
charToSquare 'T' = T
charToSquare '.' = Empty
charToSquare a = error $ "Unknown board character: " ++ [a]

parseBoard :: [String] -> [[Square]]
parseBoard = map (map charToSquare)

sections :: [a] -> [[a]]
sections xs = map tail $ chunksOf 5 xs

pathStatus :: [Square] -> GameStatus
pathStatus [] = Draw
pathStatus squares = fromMaybe Draw status
    where status = foldr newPathStatus Nothing squares

newPathStatus :: Square -> Maybe GameStatus -> Maybe GameStatus
newPathStatus Empty _ = Just Unfinished
newPathStatus _ (Just Unfinished) = Just Unfinished
newPathStatus _ (Just Draw) = Just Draw
newPathStatus T (Just (Win a)) = Just (Win a)
newPathStatus (Piece a) Nothing = Just (Win a)
newPathStatus T Nothing = Nothing
newPathStatus (Piece a) (Just (Win b)) = Just (if a == b then Win a else Draw)

finalGameStatus :: [GameStatus] -> GameStatus
finalGameStatus [] = Draw
finalGameStatus pathStatuses = foldr newGameStatus Draw pathStatuses

newGameStatus :: GameStatus -> GameStatus -> GameStatus
newGameStatus (Win a) _ = Win a
newGameStatus Draw a = a
newGameStatus Unfinished Draw = Unfinished
newGameStatus Unfinished a = a

solve :: Board -> GameStatus
solve board = finalGameStatus (map pathStatus (pathsThroughGrid board))

unMarshal :: [String] -> [Board]
unMarshal = map parseBoard . sections

caseLine :: Int -> GameStatus -> String
caseLine n status = printf "Case #%d: %s" n (show status)

marshal :: [GameStatus] -> [String]
marshal statuses = map (uncurry caseLine) $ enumerateFrom 1 statuses

main :: IO ()
main = interact $ unlines . marshal . map solve . unMarshal . lines
