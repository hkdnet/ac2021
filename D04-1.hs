import Data.List
import Data.Maybe
import System.IO
import Text.Read

type Board = [[Int]]

type Input = ([Int], [Board])

type Output = ([Int], Board)

lToInts :: String -> [Int]
lToInts line = map read $ words line

readBoard :: IO Board
readBoard = do
  _ <- getLine
  l1 <- getLine
  l2 <- getLine
  l3 <- getLine
  l4 <- getLine
  l5 <- getLine
  let lines = [l1, l2, l3, l4, l5]
  let b = map lToInts lines
  return b

readBoards :: IO [Board]
readBoards = do
  done <- isEOF
  if done
    then return []
    else do
      b <- readBoard
      bs <- readBoards
      return (b : bs)

wrap :: a -> IO a
wrap a = do
  return a

getInput :: IO Input
getInput = do
  numLine <- getLine
  let nums = map read $ wordsWhen (== ',') numLine
  boards <- readBoards
  return (nums, boards)

main :: IO ()
main = do
  (used, winBoard) <- solve $ getInput
  let ans = score used winBoard
  print ans

score :: [Int] -> Board -> Int
score nums b = (last nums) * (sum unmarked)
  where
    unmarked = filter (not . (\a -> elem a nums)) (flatten b)

solve :: IO Input -> IO Output
solve wrapped = do
  input <- wrapped
  return $ pureSolve input

pureSolve :: Input -> Output
pureSolve (nums, boards) = fromJust ans
  where
    f called = case bingos of
      [] -> Nothing
      _ -> Just (called, head bingos)
      where
        bingos = filter (isBingo called) boards
    turns = map (\c -> take c nums) [1 ..]
    Just ans = find isJust $ map f turns

isBingo :: [Int] -> Board -> Bool
isBingo called b = any (\line -> length (unmarked called line) == 0) lines
  where
    lines = linesOf b

unmarked :: [Int] -> [Int] -> [Int]
unmarked called = filter (not . (\n -> elem n called))

-- no crosses!
linesOf :: Board -> [[Int]]
linesOf b = flatten [horizontals, verticals]
  where
    horizontals = b
    verticals = transpose b

flatten :: [[a]] -> [a]
flatten [] = []
flatten (a : as) = a ++ flatten as

wordsWhen :: (Char -> Bool) -> String -> [String]
wordsWhen p s = case dropWhile p s of
  "" -> []
  s' -> w : wordsWhen p s''
    where
      (w, s'') = break p s'
