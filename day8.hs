import Data.List
import Data.Functor
import Control.Applicative
import Data.Sequence (Seq, (<|), (|>), (><))
import qualified Data.Sequence as Seq
import Data.Either
import Data.Maybe
import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L
import Data.Void

data Instruction = Acc Int | Jmp Int | Nop Int | Hal deriving Show
data InstructionV = Visited Instruction | Unvisited Instruction deriving Show
type Register = Int
type Memory   = Seq InstructionV
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
    ins = unwrap $ Seq.index m ip
    m'    = (Seq.update ip (Visited ins) m) 
    h (Nop n) = (Machine acc (succ ip) m')
    h (Acc n) = (Machine (acc + n) (succ ip) m')
    h (Jmp n) = (Machine acc (ip + n) m')
  in h ins
    
getAcc1 :: Machine -> Register 
getAcc1 ma@(Machine acc ip m) = 
  if
    (isVisited $ Seq.index m ip)
  then 
    acc
  else getAcc1 (doStep ma)

getAcc2 :: Machine -> Maybe Register 
getAcc2 ma@(Machine acc ip m) = 
  if
    ip < 0 || ip > Seq.length m - 1 || (isVisited $ Seq.index m ip)
  then 
    Nothing
  else 
    if   (isHal $ unwrap $ Seq.index m ip)
    then Just acc
    else getAcc2 (doStep ma)

getAllPrograms :: Memory -> [Memory]
getAllPrograms m = do
  i <- [0..Seq.length m - 1]
  let ins = unwrap $ Seq.index m i
  let 
    isFlippable (Jmp _) = True
    isFlippable (Nop _) = True
    isFlippable _       = False
    flipIns (Jmp a) = Nop a
    flipIns (Nop a) = Jmp a
   in
    if isFlippable ins then [Seq.update i (Unvisited $ flipIns ins) m] else []

main = do
  f <- readFile "inputs/day8.txt"
  let ls = lines f
  let instructions = ((Unvisited . fromRight undefined . toInstruction) <$> ls) ++ [Unvisited Hal]
  let memory = Seq.fromList instructions
  -- pt. 1
  let initm = (Machine 0 0 memory)
  print $ getAcc1 initm
 
  --pt. 2
  let mems = getAllPrograms memory
  let machines = (Machine 0 0) <$> mems
  print $ fromJust $ fromJust $ find (isJust) $ getAcc2 <$> machines
   
