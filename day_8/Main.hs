import Data.List

rotateN :: Int -> [[a]] -> [[a]]
rotateN n s = last (take (n + 1) (iterate rotateOne s))
  where
    rotateOne s = map reverse (transpose s)

orRows :: [[[Bool]]] -> [[Bool]]
orRows x = map orRow (transpose x)
  where orRow y = map or (transpose y)

mapRow :: Int -> String -> [Bool]
mapRow high [] = []
mapRow high (a:rs)
 | parsedA > high = True:mapRow parsedA rs
 | otherwise = False:mapRow high rs
 where parsedA = read [a] :: Int

mapRows :: [String] -> [[Bool]]
mapRows = map (mapRow (-1))

main :: IO ()
main = do
  contents <- readFile "data.txt"
  let matrix = lines contents
  let left = mapRows matrix
  let top = (rotateN 3 . mapRows . rotateN 1) matrix
  let right = (rotateN 2 . mapRows . rotateN 2) matrix
  let bottom = (rotateN 1 . mapRows . rotateN 3) matrix
  let result = orRows [left, top, right, bottom]
  print $ sum (map (length . filter id) result)