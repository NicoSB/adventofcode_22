import Control.Monad ()
import System.Environment (getArgs)
import System.IO ()

main :: IO ()
main = do
  args <- getArgs
  result <- do getMaxFood (head args)
  print result

getMaxFood :: String -> IO Int
getMaxFood filePath = do
  content <- readFile filePath
  let linesOfFiles = lines content
  return (getMaxSum 0 0 linesOfFiles)

getMaxSum :: Int -> Int -> [String] -> Int
getMaxSum current currentMax [] = max current currentMax
getMaxSum current currentMax ("" : rest) =
  let newMax = max current currentMax
   in getMaxSum 0 newMax rest
getMaxSum current currentMax (currentValue : rest) =
  let x = read currentValue :: Int
      newMax = max current currentMax
   in getMaxSum (current + x) newMax rest
