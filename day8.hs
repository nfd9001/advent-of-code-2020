import Data.List
import Data.Functor
import Control.Applicative
import Data.Array
import Data.Either
import Data.Maybe
import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L
import Data.Void

data Instruction = Acc Integer | Jmp Integer | Nop Integer | Hal deriving Show
data InstructionV = Visited Instruction | Unvisited Instruction deriving Show
type Register = Integer
type Memory   = Array Register InstructionV
data Machine  = Machine Register Register Memory --acc, ip

unwrap (Visited a) = a
unwrap (Unvisited a) = a

isVisited (Visited _) = True
isVisited _           = False

isHal Hal = True
isHal _   = False

type Parser = Parsec Void String

sc :: Parser ()
sc = L.space space1 empty empty 

lexeme :: Parser a -> Parser a
lexeme = L.lexeme sc

number = L.signed sc L.decimal

pAcc = string "acc " $> Acc
pJmp = string "jmp " $> Jmp
pNop = string "nop " $> Nop

pInstruction :: Parser Instruction
pInstruction = (pAcc <|> pJmp <|> pNop) <*> number <* eof

toInstruction = runParser pInstruction ""

doStep (Machine acc ip m) =
  let
    ins = unwrap $ m ! ip
    h (Nop n) = (Machine acc (succ ip) (m//[(ip, Visited ins)]))
    h (Acc n) = (Machine (acc + n) (succ ip) (m//[(ip, Visited ins)]))
    h (Jmp n) = (Machine acc (ip + n) (m//[(ip, Visited ins)]))
  in h ins
    
getAcc1 :: Machine -> Register 
getAcc1 ma@(Machine acc ip m) = 
  if
    (isVisited $ m!ip)
  then 
    acc
  else getAcc1 (doStep ma)

getAcc2 :: Machine -> Maybe Register 
getAcc2 ma@(Machine acc ip m) = 
  if
    ip < 0 || ip > (snd $ bounds m) || (isVisited $ m!ip)
  then 
    Nothing
  else 
    if   (isHal $ unwrap $ m!ip)
    then Just acc
    else getAcc2 (doStep ma)

getAllPrograms :: Memory -> [Memory]
getAllPrograms m = do
  i <- [0..(snd $ bounds m)]
  let ins = unwrap $ m!i
  let 
    isFlippable (Jmp _) = True
    isFlippable (Nop _) = True
    isFlippable _       = False
    flipIns (Jmp a) = Nop a
    flipIns (Nop a) = Jmp a
   in
    if isFlippable ins then [m//[(i,Unvisited $ flipIns ins)]] else []

main = do
  f <- readFile "inputs/day8.txt"
  let ls = lines f
  let instructions = ((Unvisited . fromRight undefined . toInstruction) <$> ls) ++ [Unvisited Hal]
  let memory = listArray (0::Integer, (fromIntegral $ length instructions) - 1) instructions
  -- pt. 1
  let initm = (Machine 0 0 memory)
  print $ getAcc1 initm
 
  --pt. 2
  let mems = getAllPrograms memory
  let machines = (Machine 0 0) <$> mems
  print $ fromJust $ fromJust $ find (isJust) $ getAcc2 <$> machines
   
