import Control.Monad
import Data.Char
import Data.List
import System.Environment
import System.IO

main :: IO ()
main = do
  args <- getArgs
  lines <- do getLines (head args)
  let result = sum $ getLineVal lines
  print result

getLines :: String -> IO [String]
getLines filePath = do
  content <- readFile filePath
  return (lines content)

getLineVal :: [String] -> [Int]
getLineVal = sumGroupOfThree []

sumGroupOfThree :: [[Char]] -> [String] -> [Int]
sumGroupOfThree [] (current : tail) = sumGroupOfThree [current] tail
sumGroupOfThree [a] (current : tail) = sumGroupOfThree [a, current] tail
sumGroupOfThree [a, b] (current : tail) =
  let val = getCharValue (head (intersect a b `intersect` current))
   in val : sumGroupOfThree [] tail
sumGroupOfThree _ [] = []

getCharValue :: Char -> Int
getCharValue ch =
  if isLower ch
    then ord ch - 96
    else ord ch - 38
