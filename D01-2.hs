import System.IO
import Control.Monad
import Text.Read

gi :: IO [Int] -> IO [Int]
gi s = do
  n <- getLine
  arr <- s
  case readMaybe n of
      Nothing -> do
        return $ reverse arr
      Just n  -> do
        let nx = n : arr
        done <- isEOF
        if done
          then
            return nx
          else
            gi $ wrap nx

wrap :: [Int] -> IO[Int]
wrap a = do
  return a
getInput :: IO [Int]
getInput = do
  let w = wrap []
  gi w

main :: IO ()
main = do
  ans <- solve $ getInput
  print ans

solve :: IO [Int] -> IO Int
solve wrapped = do
  nums <- wrapped
  let ans = pureSolve nums
  return ans

pureSolve :: [Int] -> Int
pureSolve nums = ans
  where
    n1 = drop 1 nums
    n2 = drop 2 nums
    sliced = zip3 nums n1 n2
    summarized = map (\(a, b, c) -> a + b + c) sliced
    zipped = zip summarized $ drop 1 summarized
    ans = foldr f 0 zipped
    f (x, y) acc = if x <= y then acc else acc + 1
