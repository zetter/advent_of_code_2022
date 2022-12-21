import Data.List
import Data.List.Split (chunksOf, splitOn)

mapWithIndex :: ((Int, a) -> b) -> [a] -> [b]
mapWithIndex f a = map f (zip [0 ..] a)

run :: Int -> [String] -> [Int]
run x [] = []
run x (i : is)
  | i == "noop" = x : run x is
  | otherwise = x : x : run (x + num) is
  where
    num = read (last (splitOn " " i)) :: Int

pixel :: (Int, Int) -> Char
pixel (cycle, x)
  | cycle <= x + 1 && cycle >= x - 1 = '#'
  | otherwise = '.'

main :: IO ()
main = do
  contents <- readFile "data.txt"
  let instructions = lines contents
  let trace = run 1 instructions
  let cycles = [20, 60, 100, 140, 180, 220]
  print $ sum (map (\i -> i * trace !! (i - 1)) cycles)
  putStrLn $ intercalate "\n" (map (mapWithIndex pixel) (chunksOf 40 trace))