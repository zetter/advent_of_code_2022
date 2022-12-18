import Data.List
import Data.List.Split (splitOn)

parseFiles :: [String] -> [String] -> [([String], Int)]
parseFiles [] _ = []
parseFiles (l : ls) base
  | "$ cd .." == l = parseFiles ls (init base)
  | "$ cd " `isPrefixOf` l = (cdDir, 0) : parseFiles ls cdDir
  | "$ ls" == l = parseFiles ls base
  | "dir " `isPrefixOf` l = parseFiles ls base
  | otherwise = (filePath, fileSize) : parseFiles ls base
  where
    cdDir = base ++ [last (splitOn " cd " l)]
    fileName = last (splitOn " " l)
    filePath = base ++ [fileName]
    fileSize = read (head (splitOn " " l))

calculateSizes :: [([String], Int)] -> [([String], Int)] -> [([String], Int)]
calculateSizes [] _ = []
calculateSizes ((dir, _) : dirs) files = (dir, size) : calculateSizes dirs files
  where
    filesInDir = filter ((dir `isPrefixOf`) . fst) files
    size = sum (map snd filesInDir)

main :: IO ()
main = do
  contents <- readFile "data.txt"
  let items = parseFiles (lines contents) []
  let (directories, files) = partition ((== 0) . snd) items
  let directorySizes = calculateSizes directories files
  print $ sum (map snd (filter ((<= 100000) . snd) directorySizes))
