import Control.Monad
import Data.Char
import Data.List
import System.Environment
import System.IO

main :: IO ()
main = do
  args <- getArgs
  lines <- do getLines (head args)
  let result = sum $ map getLineVal lines
  print result

getLines :: String -> IO [String]
getLines filePath = do
  content <- readFile filePath
  return (lines content)

getLineVal :: String -> Int
getLineVal line =
  let (a, b) = getCompartments line
   in getCharValue . head $ intersect a b

getCompartments :: String -> ([Char], [Char])
getCompartments line =
  let compartmentLength = length line `div` 2
      a = take compartmentLength line
      b = drop compartmentLength line
   in (a, b)

getCharValue :: Char -> Int
getCharValue ch =
  if isLower ch
    then ord ch - 96
    else ord ch - 38
