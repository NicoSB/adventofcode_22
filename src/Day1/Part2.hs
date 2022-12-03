import Control.Monad
import System.Environment
import System.IO

main :: IO ()
main = do
  args <- getArgs
  result <- do getMaxFood (head args)
  print (sum result)

getMaxFood :: String -> IO [Int]
getMaxFood filePath = do
  content <- readFile filePath
  let linesOfFiles = lines content
  return (getMaxSum 0 [] linesOfFiles)

getMaxSum :: Int -> [Int] -> [String] -> [Int]
getMaxSum current stack [] = insert current stack
getMaxSum current stack ("" : rest) =
  getMaxSum 0 (insert current stack) rest
getMaxSum current stack (currentValue : rest) =
  let x = read currentValue :: Int
   in getMaxSum (current + x) stack rest

insert :: Int -> [Int] -> [Int]
insert val [] = [val]
insert val [a] = if val > a then [val, a] else [a, val]
insert val [a, b]
  | val > a = [val, a, b]
  | val > b = [a, val, b]
  | otherwise = [a, b, val]
insert val [a, b, c]
  | val > a = [val, a, b]
  | val > b = [a, val, b]
  | val > c = [a, b, val]
  | otherwise = [a, b, c]
