import Data.List
import Data.Functor
import Control.Applicative hiding (many)
import Data.Either
import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L
import Data.Void
import Control.Monad
import Control.Monad.Combinators
import Control.Monad.Combinators.Expr

-- ###########################################################################
-- data
-- ###########################################################################
type Parser = Parsec Void String
-- neatly accepts representation as an initial algebra with
-- a little unwrapping cheat
data Alg
  = Num   Integer
  | Plus  Alg Alg
  | Splat Alg Alg deriving (Show)

reduce :: Alg -> Integer
reduce (Num i)    = i
reduce (Plus i j) = (reduce i) + (reduce j)
reduce (Splat i j) = (reduce i) * (reduce j)

sc :: Parser ()
sc = L.space space1 empty empty 
lexeme :: Parser a -> Parser a
lexeme = L.lexeme sc
parens :: Parser a -> Parser a
parens = between (char '(') (char ')')
pNum   = lexeme $ Num <$> (L.decimal)
symbol = L.symbol sc

-- ###########################################################################
-- pt. 1
-- ###########################################################################
expr1 = makeExprParser term1 table1
term1 = lexeme $ parens expr1 <|> pNum
table1 = [[InfixL $ (\a b -> Plus  a b) <$ symbol "+",
         InfixL $ (\a b -> Splat a b) <$ symbol "*"]]
s1 s = fromRight undefined $ runParser expr1 "" s
-- ###########################################################################
-- pt. 2
-- ###########################################################################
expr2 = makeExprParser term2 table2
term2 = lexeme $ parens expr2 <|> pNum
table2 = [[InfixL $ (\a b -> Plus  a b) <$ symbol "+"],
         [InfixL $ (\a b -> Splat a b) <$ symbol "*"]]
s2 s = fromRight undefined $ runParser expr2 "" s
-- ###########################################################################
main = do
  f <- readFile "inputs/day18.txt"
  let ls = lines f
  let soln1 = s1 <$> lines f
  print $ sum $ reduce <$> soln1
  let soln2 = s2 <$> lines f
  print $ sum $ reduce <$> soln2
  
