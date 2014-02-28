import Data.List
import Data.List.Split


data Player = X | O deriving (Eq, Show)
data Square = Piece Player | T | Empty deriving (Eq, Show)
type Board = [[Square]]
data GameStatus = Win Player | Draw | Unfinished | Unknown deriving (Eq, Show)


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

parseProblem :: String -> [Board]
parseProblem problem = map parseBoard $ sections $ lines problem

pathStatus :: [Square] -> GameStatus
pathStatus squares =
    if status == Unknown
    then Draw
    else status
        where status = foldl newPathStatus Unknown squares

newPathStatus :: GameStatus -> Square -> GameStatus
newPathStatus Unfinished _ = Unfinished
newPathStatus Draw _ = Draw
newPathStatus _ Empty = Unfinished
newPathStatus (Win a) T = Win a
newPathStatus Unknown (Piece a) = Win a
newPathStatus Unknown T = Unknown
newPathStatus (Win a) (Piece b) =
    if a == b
    then Win a
    else Draw

finalGameStatus :: [GameStatus] -> GameStatus
finalGameStatus statuses =
    if gameStatus == Unknown
    then Draw
    else gameStatus
        where gameStatus = foldr newGameStatus Unknown statuses

newGameStatus :: GameStatus -> GameStatus -> GameStatus
newGameStatus Unknown a = a
newGameStatus Draw Draw = Draw
newGameStatus Draw a = a
newGameStatus Unfinished Draw = Unfinished
newGameStatus Unfinished Unfinished = Unfinished
newGameStatus a Unfinished = a
newGameStatus Unfinished a = a
newGameStatus (Win a) _ = Win a

solve :: Board -> GameStatus
solve board = finalGameStatus (map pathStatus (paths board))

solveAll :: String -> [GameStatus]
solveAll problem = map solve (parseProblem problem)

main :: IO ()
main = do
    problem <- readFile "example.input"
    putStr $ show $ solveAll problem
