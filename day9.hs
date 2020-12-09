import Data.List
import Data.Set
import Data.Maybe
l :: [Integer] -> Integer
l xs@(j:js) = let
  (ns, x') = Data.List.splitAt 25 xs
  x = head x'
  nsSet = fromList ns
  t = do
        i <- ns
        if i /= (x - i) && (not $ member (x - i) nsSet) then [] else [i]  
  in if Data.List.null t then x else l js

-- https://stackoverflow.com/a/5377754
continuousSubSeqs = Data.List.filter (not . Data.List.null) . concatMap inits . tails

main = do
  f <- readFile "inputs/day9.txt"
  let ls = lines f
  let ints = (read <$> ls) :: [Integer]
  let p1 = l ints 
  print p1
  let ints2 = takeWhile (/= p1) ints
  let subsqs = continuousSubSeqs ints2
  let ans = fromJust $ find (\x -> sum x == p1) subsqs
  print $ (minimum ans) + (maximum ans)

