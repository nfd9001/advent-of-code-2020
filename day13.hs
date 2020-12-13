import Data.List
import Data.Functor
import Control.Applicative
import Data.Either
import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L
import Data.Void
import Control.Monad
import Control.Monad.Combinators

-- ===========================================================================
-- Data
-- ===========================================================================
type Parser = Parsec Void String
data Entry = ID Integer | X deriving (Show, Eq, Ord)

parseval = (ID <$> L.decimal) <|> (char 'x' $> X)

pEntries :: Parser [Entry]
pEntries = parseval `sepBy` char ','

getEntries = runParser pEntries ""

-- ===========================================================================
-- pt. 1 helpers
-- ===========================================================================
getArrivals (ID n) = [0, n..]
getArrivals X      = []

isID (ID _) = True
isID _      = False

firstAvailable t l = 
  let
    id = l!!1
    a  = head $ dropWhile (<t) l
  in (a, id)

-- ===========================================================================
-- pt. 2
-- ===========================================================================
fromID (ID n, t) = (n, t)
fromID _      = error "don't fromID an X"

hasTime :: Integer -> (Entry, Integer) -> Bool
hasTime t (ID i, offset) = ((offset + t) `mod` i) == 0

solve t stepBy l@(i:is) = 
  if hasTime t i 
  then solve t (stepBy * (fst $ fromID i)) is
  else solve (t+stepBy) stepBy l
solve t _ [] = t

-- ===========================================================================
main = do
  f <- readFile "inputs/day13.txt"
  let ls = lines f
  let time = (read $ ls!!0) :: Integer
  let entries = fromRight undefined (getEntries $ ls!!1)

  let ids = filter (isID) entries
  let arrivals = getArrivals <$> ids
  let (a, id) = head $ sort $ firstAvailable time <$> arrivals
  print $ id * (a - time)
  
  let idAndOffset = sortOn fst $ filter (isID . fst) (zip entries [0..])
  print $ solve 1 1 idAndOffset
  
