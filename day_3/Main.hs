import Data.List (intersect)
import Data.Char (ord)
import Data.List.Split (chunksOf)

value :: Char -> Int
value v | elem v ['a'..'z'] = ord v - 96
        | elem v ['A'..'Z'] = ord v - 38

priority :: (String, String) -> Int
priority (c1, c2) = head (map value (intersect c1 c2))

sharedPriority :: [String] -> Int
sharedPriority rucksacks = head (map value (foldl1 intersect rucksacks))

parseLine :: String -> (String, String)
parseLine rucksack = splitAt (div (length rucksack) 2) rucksack

main = do
  contents <- readFile "data.txt"
  let priorities = map (priority . parseLine) (lines contents)
  let sharedPriorities = map sharedPriority (chunksOf 3 (lines contents))
  print $ sum priorities
  print $ sum sharedPriorities
