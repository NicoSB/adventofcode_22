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
  let stack = buildStack lines
  print $ map head stack

getLines :: String -> IO [String]
getLines filePath = do
  content <- readFile filePath
  return (lines content)

buildStack :: [String] -> [[Char]]
buildStack lines =
    let (stacks, commands, length) = separateLines lines
        stacksInverted = map (filterLine length) stacks
        sts = transpose stacksInverted
        filtered = map (filter (not . isSpace)) sts
    in applyCommands (trace ("filtered" ++ show filtered) filtered) (trace ("commands" ++ show commands) commands)
        
applyCommands :: [[Char]] -> [String] -> [[Char]] 
applyCommands stack [] = stack
applyCommands stack (current:rest) =
  let split = words current
      count = read (head split) :: Int
      from = (read (split !! 1) :: Int) - 1
      to = (read (split !! 2) :: Int) - 1
  in let toMove = (take count (stack !! trace ("to " ++ show to) from))
         newStack = map (\(st, idx) -> if idx == from then drop count st else if idx == to then toMove ++ st else st) (zip stack [0..])
      in applyCommands (traceShow newStack newStack) rest

separateLines :: [String] -> ([String], [String], Int)
separateLines lines =
    let stack = takeWhile (\line -> head line /= '$') lines
        commands = drop (length stack + 2) lines
        numberLine = dropWhileEnd isSpace (lines !! (length stack + 1))
        number = read [last numberLine] :: Int
    in (stack, commands, number)

filterLine :: Int -> String  -> [Char]
filterLine numberOfStacks line =
  (map (`getVal` line) [0..(numberOfStacks - 1)])

getVal :: Int -> String -> Char
getVal idx line =
    line !! (idx * 4 + 1)
  
  
