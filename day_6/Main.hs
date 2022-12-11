
import Data.List

findMarkerRec :: Int -> Int -> String -> Int
findMarkerRec len count xs
  | nub nextX == nextX = count
  | otherwise = findMarkerRec len (count + 1) (tail xs)
  where nextX = take len xs

findMarker x = findMarkerRec x x

main :: IO ()
main = do
  contents <- readFile "data.txt"
  print $ findMarker 4 contents
  print $ findMarker 14 contents