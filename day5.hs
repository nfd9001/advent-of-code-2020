import Data.List

data Dir = Low | High deriving Show

toDir 'F' = Low
toDir 'B' = High
toDir 'L' = Low
toDir 'R' = High

getRow l = binPart 0 127 l
getCol l = binPart 0 7   l
getID (r, c) = 8 * getRow r + getCol c

binPart :: Integer -> Integer -> [Dir] -> Integer
binPart min _   (Low:[])  = min 
binPart _   max (High:[]) = max
binPart min max (Low:xs)  = binPart min (max - ((max - min + 1) `div` 2)) xs
binPart min max (High:xs) = binPart (min + ((max - min + 1) `div` 2)) max xs

main = do
  f <- readFile "inputs/day5.txt"
  let ls       = lines f
  let entries1 = (fmap toDir) <$> ls
  let segs     = splitAt 7 <$> entries1
  let allseats = sort $ getID <$> segs
  let minseat = head allseats
  let maxseat = last allseats
  print maxseat
  print $ head $ ([minseat..maxseat] \\ allseats)
