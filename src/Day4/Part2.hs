import Control.Monad
import Data.Char
import Data.List
import Data.Text (splitOn, Text, pack, unpack)
import System.Environment
import System.IO
import qualified Data.Binary.Builder as Text
import Debug.Trace (traceShow, trace)

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
  let (a, b) = parseAssignments $ pack line
   in if overlaps a b then 1 else 0

parseAssignments :: Text  -> (Assignment, Assignment)
parseAssignments line =
  let [a, b] = splitOn (pack ",") line
  
   in (parseAssignment a, parseAssignment b)

parseAssignment :: Text -> Assignment
parseAssignment raw =
    let [from, to] = splitOn (pack "-") raw
    in Assignment (read (unpack from) :: Int) (read (unpack to) :: Int)

overlaps :: Assignment -> Assignment -> Bool
overlaps a b
  | from b <= from a && to b >= from a = True
  | from a <= from b && to a >= from b = True
  | otherwise = False

data Assignment = Assignment { from :: Int, to :: Int } deriving (Show, Eq)