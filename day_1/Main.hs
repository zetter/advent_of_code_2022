import Data.List (sort)
import Data.List.Split (splitOn)

part1 = do
  contents <- readFile "data.txt"
  let groups = map (map read . lines) (splitOn "\n\n" contents)
  print (maximum (map sum groups))

main = do
  contents <- readFile "data.txt"
  let groups = map (map read . lines) (splitOn "\n\n" contents)
  print (sum (take 3 (reverse (sort (map sum groups)))))