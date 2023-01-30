import Control.Monad
import Data.List
import System.IO
import Text.Read

data Bit = T | F deriving (Eq, Show)

type Bits = [Bit]

type InputItem = Bits

type Input = [InputItem]

type Output = (Bits, Bits)

gi :: IO Input -> IO Input
gi s = do
  n <- getLine
  let bits = map (\c -> if c == '1' then T else F) n
  arr <- s
  let nx = bits : arr
  done <- isEOF
  if done
    then return $ reverse nx
    else gi $ wrap nx

wrap :: a -> IO a
wrap a = do
  return a

getInput :: IO Input
getInput = do
  let w = wrap []
  gi w

main :: IO ()
main = do
  (o2, co2) <- solve $ getInput
  let ans = (bitsToInt o2) * (bitsToInt co2)
  print ans

bitsToInt :: Bits -> Int
bitsToInt bs = f 0 1 $ reverse bs
  where
    f a _ [] = a
    f a b (bi : rest) = f (a + if bi == T then b else 0) (b * 2) rest

solve :: IO Input -> IO Output
solve wrapped = do
  arr <- wrapped
  let ans = pureSolve arr
  return ans

pureSolve :: Input -> Output
pureSolve arr = (o2, co2)
  where
    o2 = collect popular arr
    co2 = collect nonPopular arr

collect :: (Bits -> Bit) -> [Bits] -> [Bit]
collect _ ([] : _) = []
collect _ (a : []) = a
collect extractor arr = (p : collect extractor nextArr)
  where
    p = extractor firstBits
    firstBits = map head arr
    selected = filter (\(a : _) -> a == p) arr
    nextArr = map tail selected

opposite :: Bit -> Bit
opposite T = F
opposite F = T

countif :: (a -> Bool) -> Int -> [a] -> Int
countif f c [] = c
countif f c (a : rest) = countif f (if f a then c + 1 else c) rest

tCount :: [Bit] -> Int
tCount = countif (T ==) 0

fCount :: [Bit] -> Int
fCount = countif (F ==) 0

popular :: Bits -> Bit
popular arr = if (tCount arr) >= (fCount arr) then T else F

nonPopular :: Bits -> Bit
nonPopular arr = if (tCount arr) >= (fCount arr) then F else T
