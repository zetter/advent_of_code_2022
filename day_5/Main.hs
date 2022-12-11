import Data.List.Split (chunksOf, splitOn)
import Data.List

parseLine :: String -> String
parseLine line = map (head . (\\ "[]") . nub ) (chunksOf 4 line)

parseState :: [String] -> [String]
parseState lines = map (dropWhile (==' ')) (transpose (map parseLine lines))

parseMove :: String -> (Int, Int, Int)
parseMove move =  parse (splitOn " " move)
  where parse [_, count, _, from, _, to] = (read count, read from, read to)

mapWithIndex :: ((Int, a) -> b) -> [a] -> [b]
mapWithIndex f a = map f (zip [1..] a)

move :: [String] -> [(Int, Int, Int)] -> [String]
move state [] = state
move state ((0,_,_):ms) = move state ms
move state ((count,from,to):ms) = move (putOn to (takeOff from state)) ((count - 1, from, to):ms)
  where 
    takeOff from = mapWithIndex (\(i, cs) -> if i == from then tail cs else cs)
    putOn to = mapWithIndex (\(i, cs) -> if i == to then toMove:cs else cs)
    toMove = head (state !! (from - 1))

move' :: [String] -> [(Int, Int, Int)] -> [String]
move' state [] = state
move' state ((count,from,to):ms) = move' (putOn to (takeOff from state)) ms
  where 
    takeOff from = mapWithIndex (\(i, cs) -> if i == from then drop count cs else cs)
    putOn to = mapWithIndex (\(i, cs) -> if i == to then toMove ++ cs else cs)
    toMove = take count (state !! (from - 1))

main :: IO ()
main = do
  contents <- readFile "data.txt"
  let stateData = takeWhile (not . isPrefixOf " 1 ") (lines contents)
  let movesData = filter (isPrefixOf "move ") (lines contents)
  let state = parseState stateData
  let moves = map parseMove movesData
  let newState = move state moves 
  let newState' = move' state moves 
  print $ map head newState
  print $ map head newState'