import Control.Monad
import System.Environment
import System.IO

main :: IO ()
main = do
  args <- getArgs
  lines <- do getLines (head args)
  let result = sum (map getMatchScore lines)
  print result

getLines :: String -> IO [String]
getLines filePath = do
  content <- readFile filePath
  return (lines content)

getMatchScore :: String -> Int
getMatchScore line =
  let lineWords = words line
      theirs = head (head lineWords)
      ours = head (lineWords !! 1)
      shapeScore = getShapeScore ours
      resultScore = getResultScore theirs ours
   in shapeScore + resultScore

getResultScore :: Char -> Char -> Int
getResultScore 'A' 'X' = 3
getResultScore 'A' 'Y' = 6
getResultScore 'A' 'Z' = 0
getResultScore 'B' 'X' = 0
getResultScore 'B' 'Y' = 3
getResultScore 'B' 'Z' = 6
getResultScore 'C' 'X' = 6
getResultScore 'C' 'Y' = 0
getResultScore 'C' 'Z' = 3

getShapeScore :: Char -> Int
getShapeScore 'X' = 1
getShapeScore 'Y' = 2
getShapeScore 'Z' = 3
