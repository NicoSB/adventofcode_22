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

separateLines :: [String] -> ([String], [String], Int)
separateLines lines =
    let stack = takeWhile (\line -> head line /= ' ') lines
        commands = drop (length stack + 2) lines
        numberLine = dropWhileEnd isSpace Text(lines !! (length stack))
        number = read last numberLine :: Int
    in (stack, commands, number)

buildStacks :: [String] -> [[Char]] -> [[Char]] =
