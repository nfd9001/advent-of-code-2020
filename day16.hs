import Data.List
import Data.List.Split hiding (sepBy)
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

type Ticket = [Int]
type Range  = (Int, Int)
data Field  = Field { name :: String,
                      range1 :: Range,
                      range2 :: Range} deriving (Eq, Show)

sc :: Parser ()
sc = L.space space1 empty empty 

lexeme :: Parser a -> Parser a
lexeme = L.lexeme sc

arbStringColon = lexeme $ manyTill anySingle (char ':')

pTicket :: Parser Ticket
pTicket = sepBy L.decimal (char ',')
pRange  = lexeme $ do
  a <- L.decimal
  char '-'
  b <- L.decimal
  pure (a,b)

pField :: Parser Field
pField = do
  name <- arbStringColon
  range1 <- pRange
  lexeme $ string "or"
  range2 <- pRange
  pure $ Field name range1 range2
  
getTicket s = fromRight undefined $ runParser pTicket "" s
getField  s = fromRight undefined $ runParser pField "" s 

-- ===========================================================================
-- pt. 1
-- ===========================================================================
fitsRange n (x,y) = n >= x && n <= y
fitsField n (Field _ x y) = fitsRange n x || fitsRange n y

validInAnyField :: Int -> [Field] -> Bool
validInAnyField n fs = any (fitsField n) fs

-- ===========================================================================
-- pt. 2
-- ===========================================================================
valid ns fs = all (flip validInAnyField fs) ns
getValidFieldsOnCol (c) fs = do
  f <- fs
  if valid c [f] then pure f else []

data Constraint = Opts [Field] | Solve Field deriving (Eq, Show)
isOpts (Opts _) = True
isOpts _        = False

--safe from output of reduceSolved; it will be bot otherwise
fromConstraint (Solve a) = a
fromConstraint _         = error "tried to unwrap an opts" 

toConstraint (o:[]) = Solve o    
toConstraint op     = Opts op

reduceSolved cs = 
  let 
    (optss, solves) = partition (isOpts . fst) (zip cs [0..])
    tryRemove (Opts os, n) (Solve s, _) = (toConstraint $ delete s os, n)
    tryRemove done@(Solve _, _) _       = done
  in if length optss == 0 then cs
     else reduceSolved $ fst <$> sortOn snd (solves ++
            do
              opts  <- optss
              pure $ foldl' (tryRemove) opts solves)

-- ===========================================================================
main = do
  f <- readFile "inputs/day16.txt"
  let ls = lines f
  let gs = filter (/=[""]) $ splitOn [""] ls

  let fs = getField <$> (head gs)
  let t  = getTicket $ (gs!!1)!!1
  let ts = getTicket <$> (tail $ gs!!2)
  let tsc = concat ts
  let s1  = snd <$> (filter (not . fst) (zip ((flip validInAnyField fs) <$> tsc) tsc))
  print $ sum s1
  
  let tsg  = filter (flip valid fs) ts
  let tsgc = transpose tsg
  let cs = toConstraint <$> (flip getValidFieldsOnCol fs) <$> tsgc
  let scs = fromConstraint <$> reduceSolved cs
  let scs' = snd <$> (filter (\(x,_) -> isInfixOf "departure" (name x)) (zip scs [0..]))
  print $ product $ (t!!) <$> scs'

