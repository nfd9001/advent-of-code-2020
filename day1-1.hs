import Data.Set

p :: [Integer] -> Set Integer -> Integer
p (n:ns) s = if member (2020 - n) s then n * (2020 - n) else p ns s

main = do
  f     <- readFile "inputs/day1.txt"
  let ns = read <$> lines f
  let s  = fromList ns 
  putStrLn $ show $ p ns s
  
