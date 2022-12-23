import Data.List

parseMove :: String -> [(Int, Int)]
parseMove command = take count (repeat move)
  where
    count = read (tail command) :: Int
    move = case head command of
      'R' -> (1, 0)
      'L' -> (-1, 0)
      'U' -> (0, 1)
      'D' -> (0, -1)

getHeadPositions :: (Int, Int) -> [(Int, Int)] -> [(Int, Int)]
getHeadPositions (x, y) [] = []
getHeadPositions (x, y) ((mX, mY) : ms) = newLocation : getHeadPositions newLocation ms
  where
    newLocation = (x + mX, y + mY)

getTailPositions :: (Int, Int) -> [(Int, Int)] -> [(Int, Int)]
getTailPositions (x, y) [] = []
getTailPositions (x, y) ((hX, hY) : hs) = newTail : getTailPositions newTail hs
  where
    newTail = (newX, newY)
    moved = abs (hX - x) > 1 || abs (hY - y) > 1
    newX = if moved then (x + signum (hX - x)) else x
    newY = if moved then (y + signum (hY - y)) else y

main :: IO ()
main = do
  contents <- readFile "data.txt"
  let moves = concat $ map parseMove $ lines contents
  let headPositions = getHeadPositions (0, 0) moves
  let tailPositions = getTailPositions (0, 0) headPositions
  print $ length $ nub tailPositions