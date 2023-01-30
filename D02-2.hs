import Control.Monad
import System.IO
import Text.Read

type InputItem = (String, Int)

type Input = [InputItem]

type Output = (Int, Int)

gi :: IO Input -> IO Input
gi s = do
  n <- getLine
  let [command, numStr] = words n
  let num = read numStr
  arr <- s
  let nx = (command, num) : arr
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
  (x, y) <- solve $ getInput
  print $ x * y

solve :: IO Input -> IO Output
solve wrapped = do
  nums <- wrapped
  let ans = pureSolve nums
  return ans

pureSolve :: Input -> Output
pureSolve cmds = f (0, 0, 0) cmds

f :: (Int, Int, Int) -> Input -> Output
f (x, y, _) [] = (x, y)
f cur@(x, y, aim) ((cmd, num) : rest) = f nx rest
  where
    nx = case cmd of
      "forward" -> (x + num, y + aim * num, aim)
      "down" -> (x, y, aim + num)
      "up" -> (x, y, aim - num)
