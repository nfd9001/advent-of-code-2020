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
import Control.Applicative.Permutations

type Parser = Parsec Void String

sc :: Parser ()
sc = L.space space1 empty empty 

lexeme :: Parser a -> Parser a
lexeme = L.lexeme sc

-- ===========================================================================
-- pt. 1
-- ===========================================================================
type DumbPassport = (Maybe String, Maybe String, Maybe String, Maybe String, Maybe String, Maybe String, Maybe String, Maybe String)

entryToDumbPassport = runParser dumbPassportParser ""

dumbPassportParser :: Parser DumbPassport
dumbPassportParser = runPermutation $ do
  let tPDN = toPermutationWithDefault Nothing
  let arbString = Just <$> manyTill anySingle spaceChar 
  byr <- tPDN $ lexeme $ string "byr:" *> arbString 
  iyr <- tPDN $ lexeme $ string "iyr:" *> arbString
  eyr <- tPDN $ lexeme $ string "eyr:" *> arbString
  hgt <- tPDN $ lexeme $ string "hgt:" *> arbString
  hcl <- tPDN $ lexeme $ string "hcl:" *> arbString
  ecl <- tPDN $ lexeme $ string "ecl:" *> arbString
  pid <- tPDN $ lexeme $ string "pid:" *> arbString
  cid <- tPDN $ lexeme $ string "cid:" *> arbString
  pure (byr, iyr, eyr, hgt, hcl, ecl, pid, cid)
checkDumbPassport ((Just _),(Just _),(Just _),(Just _),(Just _),(Just _),(Just _),_) = True
checkDumbPassport _ = False

-- ===========================================================================
-- pt. 2 
-- ===========================================================================
type BirthYear = Integer
type IssueYear = Integer
type ExpirationYear = Integer
data Height = Cm Integer | In Integer deriving Show
type HairColor = Integer 
type EyeColor = String 
type PassportID = Integer
type CountryID = String --expected Integer later

data Passport = P BirthYear IssueYear ExpirationYear
  Height HairColor EyeColor PassportID
  (Maybe CountryID) deriving Show

entryToPassport = runParser passportParser ""

--checking is external here because
passportParser :: Parser Passport
passportParser = runPermutation $ do
  let tPDN = toPermutationWithDefault Nothing
  let tP   = toPermutation

  -- thanks CharlesFrayne
  let ensure lbl pred parser = parser >>= (\x -> if pred x then pure x else parseError (FancyError 0 $ Set.singleton $ ErrorFail lbl))

  let bounds a b x = x >= a && x <= b
  let cnum a b = ensure ("numeric value out of range: " ++ show a ++ "-" ++ show b) (bounds a b)

  let arbString = Just <$> manyTill anySingle spaceChar 
  let decimal = L.decimal
  let hexadecimal = L.hexadecimal
  let number = decimal <|> hexadecimal
  let pidCheck = (count 9 digitChar) *> spaceChar
  let heightHelper = 
       let heightHelper' (Cm a) = bounds 150 193 a
           heightHelper' (In a) = bounds 59 76 a
       in  ensure "Height outside type bounds" heightHelper'

  --strictly lowercase hex so no hexDigitChar in case
  let hclCheck = (count 6 $ choice $ (digitChar):(char <$> ['a'..'f'])) *> spaceChar
  let ecls = choice $ string <$> ["amb", "blu", "brn", "gry", "grn", "hzl", "oth"]

  --Input contained no years with evil leading zeroes to trip these up
  byr <- tP $ lexeme $ string "byr:" *> (cnum 1920 2002 decimal)
  iyr <- tP $ lexeme $ string "iyr:" *> (cnum 2010 2020 decimal)
  eyr <- tP $ lexeme $ string "eyr:" *> (cnum 2020 2030 decimal)
  hgt <- tP $ lexeme $ string "hgt:" *> (heightHelper $ decimal <**> ((string "in" $> In) <|> (string "cm" $> Cm)))
  hcl <- tP $ lexeme $ string "hcl:#" *> lookAhead hclCheck *> hexadecimal 
  ecl <- tP $ lexeme $ string "ecl:" *> ecls
  pid <- tP $ lexeme $ string "pid:" *> lookAhead pidCheck *> decimal
  cid <- tPDN $ lexeme $ string "cid:" *> arbString --number expected eventually
  pure (P byr iyr eyr hgt hcl ecl pid cid)

main = do
  f <- readFile "inputs/day4.txt"
  let ls = lines f
  let entries = (++" ") . concat . intersperse " " <$> splitOn [""] ls
  let dps = (entryToDumbPassport <$> entries)
  print $ length $ filter (checkDumbPassport) $ fromRight undefined <$> dps 

  let ps = (entryToPassport <$> entries)
  let pfails = filter (isLeft) ps
  let pgoods = filter (isRight) ps
  print $ length pgoods

-- ===========================================================================
-- Because it's heavily implied we'll come back to this/extend it,
-- I'll leave these testing gadgets here. 
-- ===========================================================================
 
  --putStrLn $ "pfails:" ++ (show $ length pfails)
  --putStrLn $ "pgoods:" ++ (show $ length pgoods)
  --sequence_ $ putStrLn . errorBundlePretty . fromLeft undefined <$> pfails
  --putStrLn $ errorBundlePretty $ fromLeft undefined $ (fails!!0)
  --print $ fromRight undefined <$> dps
  --
  --putStrLn $ errorBundlePretty $ fromLeft undefined $ (pfails!!0)

  --putStrLn $ "print $ take 15 ls"
  --print $ take 15 ls
  --putStrLn $ "print $ take 5 entries"
  --print $ take 5 entries
  --let t = zip entries (entryToDumbPassport <$> entries)
  --let fails = filter (isLeft . snd) t
  --print fails
  --print $ length fails
  --putStrLn "\n\n"
  --print $ fst (fails!!0)
  --putStrLn $ errorBundlePretty $ fromLeft undefined $ snd $ (fails!!0)
  --print entries
  --print $ entryToPassport <$> entries
