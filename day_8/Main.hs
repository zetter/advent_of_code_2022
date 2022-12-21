import Data.List

rotateN :: Int -> [[a]] -> [[a]]
rotateN n s = last (take (n + 1) (iterate rotateOne s))
  where
    rotateOne s = map reverse (transpose s)

orMatrix :: [[Bool]] -> [[Bool]] -> [[Bool]]
orMatrix = zipWith (zipWith (||))

matrixProduct :: [[Int]] -> [[Int]] -> [[Int]]
matrixProduct = zipWith (zipWith (*))

rowVisibility :: Int -> String -> [Bool]
rowVisibility high [] = []
rowVisibility high (a : rs)
  | parsedA > high = True : rowVisibility parsedA rs
  | otherwise = False : rowVisibility high rs
  where
    parsedA = read [a] :: Int

rowsVisibility :: [String] -> [[Bool]]
rowsVisibility = map (rowVisibility (-1))

cellScore :: Char -> String -> Int
cellScore _ [] = 0
cellScore c (x:xs)
  | c > x     = 1 + cellScore c xs
  | otherwise = 1

rowScore :: String -> [Int]
rowScore [] = []
rowScore (x : xs) = cellScore x xs : rowScore xs

rowsScore :: [String] -> [[Int]]
rowsScore = map rowScore

main :: IO ()
main = do
  contents <- readFile "data.txt"
  let matrix = lines contents
  let left = rowsVisibility matrix
  let top = (rotateN 3 . rowsVisibility . rotateN 1) matrix
  let right = (rotateN 2 . rowsVisibility . rotateN 2) matrix
  let bottom = (rotateN 1 . rowsVisibility . rotateN 3) matrix
  let result = foldl1 orMatrix [left, top, right, bottom]
  print $ sum (map (length . filter id) result)

  let left' = rowsScore matrix
  let top' = (rotateN 3 . rowsScore . rotateN 1) matrix
  let right' = (rotateN 2 . rowsScore . rotateN 2) matrix
  let bottom' = (rotateN 1 . rowsScore . rotateN 3) matrix
  let result' = foldl1 matrixProduct [left', top', right', bottom']
  print $ maximum (map maximum result')

