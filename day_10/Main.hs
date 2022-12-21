import Data.List.Split (splitOn)

run :: Int -> [String] -> [Int]
run x [] = []
run x (i : is)
  | i == "noop" = x : run x is
  | otherwise = x : x : run (x + num) is
  where
    num = read (last (splitOn " " i)) :: Int

main :: IO ()
main = do
  contents <- readFile "data.txt"
  let instructions = lines contents
  let trace = 1 : run 1 instructions
  let cycles = [20, 60, 100, 140, 180, 220]
  print $ sum (map (\i -> i * trace !! i) cycles)