import Data.List.Split
import Data.IntMap.Strict (IntMap)
import qualified Data.IntMap.Strict as IntMap

solve m last ind stop =
 let
    lastind = IntMap.findWithDefault 0 last m
    thisval = if lastind == 0 then 0 else ((ind - 1) - lastind)
  in if ind == stop 
     then thisval
     else solve (IntMap.insert last (ind - 1) m) thisval (succ ind) stop

main = do
  f <- readFile "inputs/day15.txt"
  let l = lines f
  let is = read <$> splitOn "," (l!!0)
  let m0 = IntMap.fromList (init $ zip is [1..])

  print $ solve m0 (last is) (1 + length is) 2020
  print $ solve m0 (last is) (1 + length is) 30000000
 
