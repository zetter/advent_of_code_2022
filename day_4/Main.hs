import Data.List
import Data.List.Split (splitOneOf)

isOverlapping :: ([Int], [Int]) -> Bool
isOverlapping (r1, r2) = null (r1 \\ r2) || null (r2 \\ r1)

anyOverlap :: ([Int], [Int]) -> Bool
anyOverlap (r1, r2) = not (null (r1 `intersect` r2))

parseLine :: String -> ([Int], [Int])
parseLine line = toTuple(map read (splitOneOf "-," line))
  where toTuple [a,b,c,d]  = ([a..b],[c..d])

main :: IO ()
main = do
  contents <- readFile "data.txt"
  let shiftPairs = map parseLine (lines contents)
  let overlapping = filter isOverlapping shiftPairs
  let anyOverlaps = filter anyOverlap shiftPairs
  print $ length overlapping
  print $ length anyOverlaps