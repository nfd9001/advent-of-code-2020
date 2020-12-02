import Data.List
import Data.List.Split
xor a b = (a || b) && (not $ a && b)

p ((low, high), letter, pass) = 
  if ((low > length pass - 1) || ((pass!!(low - 1)) == letter)) `xor`
    ((low > length pass - 1)  || ((pass !! (high - 1)) == letter)) 
  then 1 else 0

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
