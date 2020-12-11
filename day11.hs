import Data.List
import Data.Functor
import Control.Applicative
import qualified Data.Set as Set
import Data.Either
import Data.Maybe
import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L
import Data.Void
import Control.Monad
import Control.Applicative.Permutations
import Data.Matrix hiding ((<|>))

type Parser = Parsec Void String
data Place = Floor | Empty | Occupied deriving (Eq, Show)

pFloor = (char '.') $> Floor
pEmpty = (char 'L') $> Empty
pOccupied = (char '#') $> Occupied

pLine :: Parser [Place]
pLine = (Text.Megaparsec.many (pFloor <|> pEmpty <|> pOccupied)) <* eof
getPlaces = runParser pLine ""

isOccupied Occupied = True
isOccupied _        = False

nextPos m (row, col) me = 
  let
    isJOccupied (Just Occupied) = True
    isJOccupied _        = False
    c1 = safeGet (row - 1) (col - 1) m
    c2 = safeGet (row - 1) col m
    c3 = safeGet (row - 1) (col + 1) m
    c4 = safeGet row (col - 1) m
    c6 = safeGet row (col + 1) m
    c7 = safeGet (row + 1) (col - 1) m
    c8 = safeGet (row + 1) col m
    c9 = safeGet (row + 1) (col + 1) m
    n Empty 0    = Occupied
    n Occupied x = if x >= 4 then Empty else Occupied
    n i _        = i 
  in n me $ length $ filter isJOccupied [c1, c2, c3, c4, c6, c7, c8, c9]

nw (a,b) = let j = (a-1, b-1) in j:(nw j)
nor (a,b) = let j = (a-1, b) in j:(nor j)
ne (a,b) = let j = (a-1, b+1) in j:(ne j)
wes (a,b) = let j = (a, b-1) in j:(wes j)
eas (a,b) = let j = (a, b+1) in j:(eas j)
sw (a,b) = let j = (a+1, b-1) in j:(sw j)
sou (a,b) = let j = (a+1, b) in j:(sou j)
se (a,b) = let j = (a+1, b+1) in j:(se j)
dirs = [nw, nor, ne, wes, eas, sw, sou, se] :: [(Int, Int) -> [(Int, Int)]]
relistHead x = if null x then [] else [head x]

nextPos2 m p@(row, col) me =
  let
    isSeat Empty    = True
    isSeat Occupied = True
    isSeat _        = False
    -- [[Place] stopping at each boundary.
    look = (fmap fromJust) <$> (takeWhile isJust) <$> (fmap (\x -> uncurry safeGet x m)) <$> (dirs <*> pure p) 
    n Empty 0    = Occupied
    n Occupied x = if x >= 5 then Empty else Occupied
    n i _        = i
  in n me $ length $ filter (isOccupied) $ (concat $ relistHead . dropWhile (\x -> not $ isSeat x) <$> look)

solve f m = 
  let
    m' = mapPos (f m) m
  in if m == m' then length $ filter (isOccupied) $ toList m else solve f m'

main = do
  f <- readFile "inputs/day11.txt"
  let ls = lines f
  let places = (fromRight undefined . getPlaces) <$> ls
  let m = fromLists places
  print $ solve nextPos m
  print $ solve nextPos2 m

