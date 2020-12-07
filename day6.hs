import Data.List
import Data.List.Split
main = do
  f <- readFile "inputs/day6.txt"
  let ls     = lines f
  let groups = splitOn [""] ls
  print $ sum $ (length . nub . concat) <$> groups
  print $ sum $ (length . foldl1' (intersect)) <$> groups

