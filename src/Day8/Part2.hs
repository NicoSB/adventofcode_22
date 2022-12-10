import Control.Monad
import Data.Char
import Data.List
import Data.Text (splitOn, Text, pack, unpack)
import System.Environment
import System.IO
import qualified Data.Binary.Builder as Text
import Debug.Trace (traceShow, trace)
import Data.Set (Set, insert, empty, fromList)

main :: IO ()
main = do
  args <- getArgs
  lines <- do getLines (head args)
  let processedLines = process lines (replicate 10 (0, 0)) []
  let set = Data.Set.fromList processedLines
  print set
  print $ length set
getLines :: String -> IO [String]
getLines filePath = do
  content <- readFile filePath
  return (lines content)

process :: [String] -> [(Int, Int)] -> [(Int, Int)] -> [(Int, Int)]
process [] _ acc = acc
process (instruction:rest) knots acc =
  let (newKnots, newAcc) = move knots instruction acc
  in process rest newKnots newAcc 

move :: [(Int, Int)] -> String -> [(Int, Int)] -> ([(Int, Int)], [(Int, Int)])
move curNots instruction acc =
  let [dir, val] = words instruction  
  in foldl update (curNots, acc) (replicate (read val) dir)
  
update :: ([(Int, Int)], [(Int, Int)]) -> String -> ([(Int, Int)], [(Int, Int)])
update (head:rest, acc) dir =
    let (knots, newTail) = updatePositions dir head rest
    in (knots, newTail:acc)

updatePositions :: String -> (Int, Int) -> [(Int, Int)] -> ([(Int, Int)], (Int, Int))
updatePositions dir head tail =
  let newHead = updateHead dir head
      (newKnots, newTail) = updateRecursive newHead tail
  in (newHead:newKnots, newTail)

updateHead :: String -> (Int, Int) -> (Int, Int)
updateHead "R" (x, y) = (x + 1, y)
updateHead "L" (x, y) = (x - 1, y)
updateHead "U" (x, y) = (x, y + 1)
updateHead "D" (x, y) = (x, y - 1)

updateRecursive :: (Int, Int) -> [(Int, Int)] -> ([(Int, Int)], (Int, Int))
updateRecursive head [] = ([], head)
updateRecursive head (current:rest) =
  let updatedSelf = updateTail head current
      (updatedSuccessors, tailPosition) = updateRecursive updatedSelf rest
  in (updatedSelf:updatedSuccessors, tailPosition)

updateTail :: (Int, Int) -> (Int, Int) -> (Int, Int) 
updateTail head tail =
    let (headX, headY) = head
        (tailX, tailY) = tail
        xDiff = headX - tailX
        yDiff = headY - tailY
    in  if abs xDiff <= 1 && abs yDiff <= 1 then tail
        else if abs xDiff > 1 && abs yDiff == 0 then (tailX + (if xDiff > 0 then 1 else -1), tailY)
        else if abs xDiff == 0 && abs yDiff > 1 then (tailX, tailY + (if yDiff > 0 then 1 else -1))
        else (tailX + (if xDiff > 0 then 1 else -1), tailY + (if yDiff > 0 then 1 else -1))
  


  

