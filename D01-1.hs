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
  let zipped = zip nums $ drop 1 nums
  let ans = foldr f 0 zipped
  return ans
  where
    f (x, y) acc = if x < y then acc else acc + 1
