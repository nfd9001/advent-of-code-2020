import Data.List
import qualified Data.IntMap.Strict as M
import Data.Array.ST
import Data.Array
import qualified Data.Array.MArray as MArray
import Data.Array.ST (runSTArray)
import Control.Monad (replicateM, forM_, when)

pairs l = zip l $ tail l

findCandidates (x:xs) = (x, takeWhile (<= x + 3) xs):(findCandidates xs)
findCandidates []     = []

-- shoutouts to this guide for a quick pattern for DP in STArray:
-- https://levelup.gitconnected.com/functional-dynamic-programming-with-haskell-top-down-and-bottom-up-7ccade222337
-- shoutouts to CharlesF again for pointing out I needed traverse and not fmap
countPaths m greatest = r Data.Array.! 0 where
  r = runSTArray $ do
    arr <- MArray.newArray (0, greatest) (toInteger$ (-1))
    writeArray arr greatest (toInteger 1)
    countPathsRec m arr 0
    return arr
countPathsRec m arr i = do
  v <- readArray arr i
  when (v == -1) $ do
    children <- traverse (countPathsRec m arr) (m M.! i)
    writeArray arr i (sum children)
  readArray arr i

main = do
  f <- readFile "inputs/day10.txt"
  let ls = lines f
  let ints = sort $ (read <$> ls) :: [Int]
  let ints' = 0:ints ++ [3 + maximum ints]
  let diffs = (uncurry (flip (-))) <$> pairs ints'
  let diffs' = group $ sort $ diffs
  print $ (length (diffs'!!0)) * (length (diffs'!!1))

  let m = M.fromList $ findCandidates ints'
  print $ (countPaths m (maximum ints)) 

