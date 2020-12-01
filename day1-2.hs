import Data.Set
import Data.List

p ns s = do
   n <- ns
   m <- ns
   let 
     t = 2020 - n - m
     pp (i:is) = if member t s then pure (n, m, t) else pp is 
     pp []  = []
   pp ns  

main = do
  f     <- readFile "inputs/day1.txt"
  let ns = sort $ read <$> lines f
  let s  = fromList ns 
  let (a,b,c) = head $ p ns s
  putStrLn $ show $ a * b * c 
  
