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

part_1 = do
  contents <- readFile "data.txt"
  let games = map parseGame (lines contents)
  print (sum (map (\game -> shapePoints game + outcomePoints game) games))
  where
    parseGame [p1, ' ', p2] = (p1, p2)

outcomePoints' :: (Char, Char) -> Int
outcomePoints' (_, 'X') = 0
outcomePoints' (_, 'Y') = 3
outcomePoints' (_, 'Z') = 6

-- lose, draw, win
shapePoints' :: (Char, Char) -> Int
shapePoints' ('A', p2) = case p2 of 'X' -> 3; 'Y' -> 1; 'Z' -> 2
shapePoints' ('B', p2) = case p2 of 'X' -> 1; 'Y' -> 2; 'Z' -> 3
shapePoints' ('C', p2) = case p2 of 'X' -> 2; 'Y' -> 3; 'Z' -> 1

main = do
  contents <- readFile "data.txt"
  let games = map parseGame (lines contents)
  print (sum (map (\game -> shapePoints' game + outcomePoints' game) games))
  where
    parseGame [p1, ' ', p2] = (p1, p2)