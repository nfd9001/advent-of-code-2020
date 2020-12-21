import Data.List
import Data.List.Split hiding (sepBy)
import Data.Functor
import Control.Applicative hiding (many)
import Data.Either
import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L
import Data.Void
import Control.Monad
import Control.Monad.Combinators
import Data.Set (Set)
import qualified Data.Set as Set

-- ===========================================================================
-- Data
-- ===========================================================================
type Parser = Parsec Void String

data Cube = Active | Inactive deriving (Eq)
pCube = (char '#' $> Active) <|> (char '.' $> Inactive)

pCLine :: Parser [Cube]
pCLine = many pCube

getCLine s = fromRight undefined $ runParser pCLine "" s


-- ===========================================================================
-- pt. 1
-- ===========================================================================
type Coord3 = (Int, Int, Int)
neighbors3 (x,y,z) = Set.fromList $ do
  x1 <- [-1..1]
  y1 <- [-1..1]
  z1 <- [-1..1]
  pure (x+x1, y+y1, z+z1)
neighborsNoN3 n = Set.delete n $ neighbors3 n

needsAttention3 :: Set Coord3 -> Set Coord3
needsAttention3 s = Set.unions $ neighbors3 <$> Set.toList s

isActive3 :: Set Coord3 -> Coord3 -> Bool
isActive3 s c =
  let
    cb  = Set.member c s
    nsb = length $ filter (flip Set.member s) (Set.toList $ neighborsNoN3 c)
  in if cb then nsb == 2 || nsb == 3 else nsb == 3

doTick3 s =
  let
    nats = Set.toList $ needsAttention3 s
  in Set.fromList $ filter (isActive3 s) nats


-- ===========================================================================
-- pt. 2
-- ===========================================================================
type Coord4 = (Int, Int, Int, Int)
neighbors4 (x,y,z,w) = Set.fromList $ do
  x1 <- [-1..1]
  y1 <- [-1..1]
  z1 <- [-1..1]
  w1 <- [-1..1]
  pure (x+x1, y+y1, z+z1, w+w1)
neighborsNoN4 n = Set.delete n $ neighbors4 n

needsAttention4 :: Set Coord4 -> Set Coord4
needsAttention4 s = Set.unions $ neighbors4 <$> Set.toList s

isActive4 :: Set Coord4 -> Coord4 -> Bool
isActive4 s c =
  let
    cb  = Set.member c s
    nsb = length $ filter (flip Set.member s) (Set.toList $ neighborsNoN4 c)
  in if cb then nsb == 2 || nsb == 3 else nsb == 3

doTick4 s =
  let
    nats = Set.toList $ needsAttention4 s
  in Set.fromList $ filter (isActive4 s) nats


-- ===========================================================================

main = do
  f <- readFile "inputs/day17.txt"
  let ls  = lines f
  let csList = getCLine <$> ls
  let len = length ls
  let hasActive = (do
        x <- [0..len-1]
        y <- [0..len-1]
        if csList!!y!!x == Active then [(x,y,0)] else []) :: [Coord3]
  let s = Set.fromList hasActive
  print $ Set.size $ (iterate doTick3 s)!!6

  let s4 = Set.fromList $ (\(x,y,z) -> (x,y,z,0)) <$> hasActive
  print $ Set.size $ (iterate doTick4 s4)!!6

