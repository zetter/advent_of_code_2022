import Data.List.Split (splitOn)

main = do
  contents <- readFile "data.txt"
  let groups = map (map read . lines) (splitOn "\n\n" contents)
  print (maximum (map sum groups))