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

import Data.Int
import Data.Bits
-- Using Map instead of IntMap for portability; if we start adding more than
-- 2**32 different writes in one structure then this will break, but call
-- that out-of-scope here
--
-- Speaking of which, even allowing system-width words or Ints feels a bit
-- smelly. Common library functions accept Int where they should probably
-- accept some typeclass for fixed-width integer types instead, which would
-- permit design like C programmers using int64_t or what have you. Better
-- luck next Prelude?
import qualified Data.Map.Strict as Map


-- ===========================================================================
-- Data
-- ===========================================================================
type Parser = Parsec Void String
type Address = Int64 -- happens to have more than 36 bits
type MWord   = Int64 -- ditto
data MaskElem    = X | One | Zero deriving (Eq, Show) -- id, setBit a pos, clearBit a pos
data Statement = Write Address MWord | Mask [MaskElem] deriving Show

pElem = (char 'X' $> X) <|> (char '1' $> One) <|> (char '0' $> Zero)
pMask = Mask <$> ((string "mask = ") *> (many pElem))
sqBraces = between (char '[') (char ']') 
pWrite = do
   (string "mem") 
   addr <- (sqBraces L.decimal) 
   (string " = ")
   val <- L.decimal
   pure $ Write addr val

pStatement :: Parser Statement
pStatement = (pMask <|> pWrite) <* eof

getStatement s = fromRight undefined $ runParser pStatement "" s

-- I really feel like this function probably already has a name; if you're
-- reading this and know what that is, please fill me in; XOXO
solve f i (x:xs) = solve f (f i x) xs
solve _ i []     = i

-- ===========================================================================
-- pt. 1
-- ===========================================================================
type MState = (Map.Map Address MWord, [MWord -> MWord])

handleStatement :: MState -> Statement -> MState
handleStatement m (Write a w) = writeS m a w
handleStatement m (Mask mes)  = setMask m mes

writeS :: MState -> Address -> MWord -> MState
writeS (mem, mas) a w = (Map.insert a (runMask w mas) mem, mas)

setMask (mem, _) mes = (mem, maskToFuns mes)

maskToFuns mes  =
  let
    p = filter ((/=X) . fst) $ zip mes [35,34..0]
    t (One, n) = flip setBit n
    t (Zero, n) = flip clearBit n
  in t <$> p

runMask n (f:fs) = runMask (f n) fs
runMask n []     = n 

-- ===========================================================================
-- pt. 2
-- ===========================================================================
type MState2 = (Map.Map Address MWord, MWord -> [Address])

handleStatement2 :: MState2 -> Statement -> MState2
handleStatement2 m (Write a w) = writeS2 m a w
handleStatement2 m (Mask mes)  = setMask2 m mes

writeS2 :: MState2 -> Address -> MWord -> MState2
writeS2 (mem, getAddrs) addr w =
  let 
    addrs = getAddrs addr
  in (solve (\m k -> Map.insert k w m) mem addrs, getAddrs)

setMask2 (mem, _) mes = (mem, getWriteAddrsForMask mes)

getWriteAddrsForMask :: [MaskElem] -> MWord -> [Address]
getWriteAddrsForMask mes add =  
  let
    p = filter ((/=Zero) . fst) $ zip mes [35,34..0]
    (floatingRules, writeOneRules) = partition ((==X) . fst) p
    writeOneFuns = (flip setBit . snd <$> writeOneRules) :: [Address -> Address]
    floatingFuns = ((
                    \offset addrs -> 
                    addrs >>= (
                    \addr -> [setBit addr offset, clearBit addr offset]))
                    . snd <$> floatingRules) :: [[Address] -> [Address]]
    addr' = solve (flip ($)) add writeOneFuns
    addrs = solve (flip ($)) [addr'] floatingFuns
  in addrs

-- ===========================================================================
main = do
  f <- readFile "inputs/day14.txt"
  let ls = lines f
  let sts = getStatement <$> ls
  let init = (Map.empty, [])
  let ans = Map.toList $ fst $ solve handleStatement init sts
  print $ sum $ snd <$> ans

  let init2 = (Map.empty, \a -> [a])
  let ans2 = Map.toList $ fst $ solve handleStatement2 init2 sts
  print $ sum $ snd <$> ans2

