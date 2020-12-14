import Data.List
import Data.Functor
import Control.Applicative
import qualified Data.Either as Either
import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L
import Data.Void
import Control.Monad

-- ===========================================================================
-- Data
-- ===========================================================================

type Parser = Parsec Void String

data Heading = North | South | East | West deriving (Eq, Show)
data Turn = TLeft | TRight deriving (Eq, Show)
data Action = T Turn | H Heading | Forward deriving (Eq, Show)
type Instruction = (Action, Int)

parseHeading   = (char 'N' $> North) <|> 
                 (char 'S' $> South) <|> 
                 (char 'E' $> East) <|> 
                 (char 'W' $> West) 
                 
parseTurn = (char 'L' $> TLeft) <|> (char 'R' $> TRight) 
parseAction  = (T <$> parseTurn) <|> (H <$> parseHeading) <|> (char 'F' $> Forward)

parseInstruction :: Parser Instruction
parseInstruction = (,) <$> parseAction <*> L.decimal <* eof
getInstruction = runParser parseInstruction ""

-- ===========================================================================
-- Helpers
-- ===========================================================================
rottimes x = x `div` 90

-- ===========================================================================
-- pt. 1
-- ===========================================================================
type Ship1 = (Heading, Int, Int)

go (h, x, y) ins@(T _, _) = ((rotate h ins), x, y)
go s (H d, x) = goH s d x
go s@(d, _, _) (Forward, x) = goH s d x

lefts  = cycle [North, West, South, East]
rights = cycle [North, East, South, West]

rotate :: Heading -> Instruction -> Heading
rotate dir ((T TLeft), x)  = head $ drop (rottimes x) $ dropWhile (/= dir) lefts
rotate dir ((T TRight), x) = head $ drop (rottimes x) $ dropWhile (/= dir) rights

goH s North = goNorth s
goH s South = goSouth s
goH s East  = goEast  s
goH s West  = goWest  s

goNorth (h, x, y) n = (h, x+n, y)
goSouth (h, x, y) n = (h, x-n, y)
goEast  (h, x, y) n = (h, x, y+n)
goWest  (h, x, y) n = (h, x, y-n)

-- ===========================================================================
-- pt. 2
-- ===========================================================================
type Waypoint = (Int, Int)
type Ship2    = (Waypoint, Int, Int)

go2 (w, x, y) ins@(H _, _)     = ((moveWaypoint   w ins), x, y)
go2 (w, x, y) ins@(T _, _)     = ((rotateWaypoint w ins), x, y)
go2  s        ins@(Forward, n) = moveShip s n 

moveWaypoint :: Waypoint -> Instruction -> Waypoint
moveWaypoint (x, y) (H North, n) = (x, y + n)
moveWaypoint (x, y) (H South, n) = (x, y - n)
moveWaypoint (x, y) (H East,  n) = (x + n, y)
moveWaypoint (x, y) (H West,  n) = (x - n, y)

rot p     0 = p
rot (x,y) 1 = ( y, -x)
rot (x,y) 2 = (-x, -y)
rot (x,y) 3 = (-y,  x)
rot (x,y) n = rot (x,y) (n + 4)

rotateWaypoint :: Waypoint -> Instruction -> Waypoint
rotateWaypoint p (T TLeft,  n) = 
  let t = (-1) * rottimes n
  in rot p t
rotateWaypoint p (T TRight, n) =
  let t = rottimes n
  in rot p t

moveShip :: Ship2 -> Int -> Ship2
moveShip ((a, b), x, y) n = ((a,b), x + n * a, y + b * n)

-- ===========================================================================
main = do
  f <- readFile "inputs/day12.txt"
  let ls = lines f
  let ins = Either.fromRight undefined . getInstruction <$> ls
  let (_, x , y ) = foldl' go  (East, 0, 0) ins
  print $ (abs x ) + (abs y )

  let (_, x1, y1) = foldl' go2 ((10,1), 0, 0) ins
  print $ (abs x1) + (abs y1)
