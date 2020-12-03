import qualified Data.Matrix as M -- matrix

h :: Int -> Int -> Int -> Int -> (Int,Int) -> [(Int, Int)]
h r c w s (rights, downs) = 
  if r > s 
  then [] 
  else (r,c):(h (r+downs) (
    if ((c+rights) > w) 
    then (c+rights) `mod` (w + 1) + 1 
    else c+rights)
    w s (rights, downs)) 

main = do
  f <- readFile "inputs/day3.txt"
  let ls = lines f
  let m = M.fromLists ls

  let path1 = h 1 1 (M.ncols m) (M.nrows m) (3,1)
  print $ length $ filter (=='#') $ (m M.!) <$> path1

  let c = [(1,1), (3,1), (5,1), (7,1), (1,2)]
  let paths2 = (h 1 1 (M.ncols m) (M.nrows m)) <$> c
  print $ product $ length . (filter (=='#') . fmap (m M.!)) <$> paths2
  
