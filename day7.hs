{-# LANGUAGE ApplicativeDo #-}

import Data.List
import Data.Functor
import Control.Applicative
import qualified Data.Set as Set
import Data.List.Split
import Data.Either
import Data.Maybe
import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L
import Data.Void
import Control.Monad
import Data.Graph
import qualified Data.Map as M

type Parser = Parsec Void String

type Color = String
type ColorNum = (Color, Integer)
type Constraint = (Color, [ColorNum])

sc :: Parser ()
sc = L.space space1 empty empty 

lexeme :: Parser a -> Parser a
lexeme = L.lexeme sc

arbString :: Parser String 
arbString = lexeme $ manyTill anySingle spaceChar 

colorParser = lexeme $ do
  w1 <- arbString
  w2 <- arbString 
  void $ string "bag" <* (optional (char 's'))
  pure $ w1 ++ (' ':w2)

noOtherBagParser = lexeme $ string "no other bags" *> pure []

colorNumParser = lexeme $ do
  n <- lexeme $ L.decimal 
  c <- colorParser 
  void $ (optional (choice [char ',', char '.']))
  pure (c, n)

constraintParser :: Parser Constraint
constraintParser = do
  fst <- lexeme $ colorParser <* string "contain"
  cns <- noOtherBagParser <|> (Text.Megaparsec.some colorNumParser)
  pure (fst, cns)
  
stringToConstraint = runParser constraintParser ""

main = do
  f <- readFile "inputs/day7.txt"
  let ls = lines f
  let constraints = (fromRight undefined . stringToConstraint) <$> ls
  --p1
  let dropN (a, x) = (a, a, fst <$> x)
  let cNoN = dropN <$> constraints
  let (graph, nFV, vFK) = graphFromEdges cNoN
  let start = fromJust $ vFK "shiny gold"
  let graph' = transposeG graph
  print $ length $ reachable graph' start
  --p2
  let s = M.fromList constraints 
  let solveChildren key = 
        let
          i = (s M.! key) 
        in 
          sum $ do
                  pair <- i
                  let key = fst pair
                  let n = snd pair
                  pure (n + n * solveChildren key)
  print $ solveChildren "shiny gold"
