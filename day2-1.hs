import Data.List
import Data.List.Split

p ((low, high), letter, pass) = let
  n = length $ filter (== letter) pass
  in if n >= low && n <= high then 1 else 0

sepEntries spls = do
    l <- spls
    let rangel = read <$> splitOn "-" (l !! 0)
    let low = rangel !! 0
    let high = rangel !! 1
    let letter = head (l !! 1)
    let pass = l!!2
    pure ((low, high), letter, pass) 

main = do
  f <- readFile "inputs/day2.txt"
  let ls = lines f
  let spls = (splitOn " ") <$> ls
  let entries = sepEntries spls 
  print $ sum $ p <$> entries
