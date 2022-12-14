import Data.List

parseMove :: String -> [(Int, Int)]
parseMove command = replicate count move
  where
    count = read (tail command) :: Int
    move = case head command of
      'R' -> (1, 0)
      'L' -> (-1, 0)
      'U' -> (0, 1)
      'D' -> (0, -1)

nextHead :: (Int, Int) -> (Int, Int) -> (Int, Int)
nextHead (x, y) (mX, mY) = (x + mX, y + mY)

nextTail :: (Int, Int) -> (Int, Int) -> (Int, Int)
nextTail (x, y) (hX, hY) = (x + mX, y + mY)
  where
    moved = abs (hX - x) > 1 || abs (hY - y) > 1
    mX = if moved then signum (hX - x) else 0
    mY = if moved then signum (hY - y) else 0

main :: IO ()
main = do
  contents <- readFile "data.txt"
  let moves = concatMap parseMove (lines contents)
  let headPositions = scanl nextHead (0, 0) moves
  let nthPositions = iterate (scanl nextTail (0, 0)) headPositions
  print $ length . nub $ nthPositions !! 1
  print $ length . nub $ nthPositions !! 9