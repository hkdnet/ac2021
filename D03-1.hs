import Control.Monad
import Data.List
import System.IO
import Text.Read

data Bit = T | F deriving (Eq)

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
  (g, e) <- solve $ getInput
  let ans = (bitsToInt g) * (bitsToInt e)
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
pureSolve arr = (epsilon, gamma)
  where
    gamma = map (\a -> if (tCount a) > (fCount a) then T else F) transposed
    epsilon = map (\a -> if a == T then F else T) gamma
    transposed = transpose arr
    countif f c [] = c
    countif f c (a : rest) = countif f (if f a then c + 1 else c) rest
    tCount = countif (\b -> b == T) 0
    fCount = countif (\b -> b == F) 0
