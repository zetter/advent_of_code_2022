outcomePoints :: (Char, Char) -> Int
outcomePoints ('A', 'Y') = 6
outcomePoints ('B', 'Z') = 6
outcomePoints ('C', 'X') = 6
outcomePoints ('A', 'X') = 3
outcomePoints ('B', 'Y') = 3
outcomePoints ('C', 'Z') = 3
outcomePoints (p1, p2) = 0

shapePoints :: (Char, Char) -> Int
shapePoints (_, 'X') = 1
shapePoints (_, 'Y') = 2
shapePoints (_, 'Z') = 3

main = do
  contents <- readFile "data.txt"
  let games = map parseGame (lines contents)
  print (sum (map (\game -> shapePoints game + outcomePoints game) games))
  where
    parseGame [p1, ' ', p2] = (p1, p2)