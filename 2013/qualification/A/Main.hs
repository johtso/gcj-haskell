import Data.List
import Data.List.Split
import Data.Maybe


data Player = X | O deriving (Eq, Show)
data Square = Piece Player | T | Empty deriving (Eq, Show)
type Board = [[Square]]
data GameStatus = Win Player | Draw | Unfinished deriving (Eq, Show)


enumerate :: [a] -> [(Int, a)]
enumerate = zip [0..]

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

paths :: [[a]] -> [[a]]
paths = concatMapApply [id, transpose, diagonals]

square :: Char -> Square
square 'X' = Piece X
square 'O' = Piece O
square 'T' = T
square '.' = Empty
square a = error $ "Unknown board character: " ++ [a]

parseBoard :: [String] -> [[Square]]
parseBoard = map (map square)

sections :: [a] -> [[a]]
sections xs = map tail $ chunksOf 5 xs

unMarshal :: String -> [Board]
unMarshal = map parseBoard . sections . lines

pathStatus :: [Square] -> GameStatus
pathStatus squares = fromMaybe Draw status
    where status = foldl newPathStatus Nothing squares

newPathStatus :: Maybe GameStatus -> Square -> Maybe GameStatus
newPathStatus (Just Unfinished) _ = Just Unfinished
newPathStatus (Just Draw) _ = Just Draw
newPathStatus _ Empty = Just Unfinished
newPathStatus (Just (Win a)) T = Just (Win a)
newPathStatus Nothing (Piece a) = Just (Win a)
newPathStatus Nothing T = Nothing
newPathStatus (Just (Win a)) (Piece b) = Just (if a == b then Win a else Draw)

finalGameStatus :: [GameStatus] -> GameStatus
finalGameStatus = foldr newGameStatus Draw

newGameStatus :: GameStatus -> GameStatus -> GameStatus
newGameStatus (Win a) _ = Win a
newGameStatus Draw a = a
newGameStatus Unfinished Draw = Unfinished
newGameStatus Unfinished a = a

solve :: Board -> GameStatus
solve board = finalGameStatus (map pathStatus (paths board))

main :: IO ()
main = do
    problem <- readFile "example.input"
    putStr $ show $ map solve (unMarshal problem)
