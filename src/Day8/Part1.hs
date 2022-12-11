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
  let field = getVisibles $ parseField lines
  -- print field
  print $ length $ filter id (concat field)
getLines :: String -> IO [String]
getLines filePath = do
  content <- readFile filePath
  return (lines content)

parseField :: [[Char]] -> [[Int]]
parseField = map (map (\c -> read [c] :: Int))

getVisibles :: [[Int]] -> [[Bool]]
getVisibles field =
  let width = length (head field)
      height = length field
  in map (\i -> map (\j -> isVisible i j height width field) (take width [0..])) (take height [0..])

isVisible :: Int -> Int -> Int -> Int -> [[Int]] -> Bool
isVisible i j height width field =
  let value = at i j field
      lefts = map (\v -> at i v field) (take (j) [0..])
      ups = map (\v -> at v j field) (take (i) [0..])
      rights = map (\v -> at i v field) (take (width - j - 1) [(j + 1)..])
      downs = map (\v -> at v j field) (take (height - i - 1) [(i + 1)..])
      canSeeFromLeft = isTallest value lefts
      canSeeFromUp = isTallest value ups
      canSeeFromRight = isTallest value rights
      canSeeFromDown = isTallest value downs

  in canSeeFromLeft || canSeeFromUp || canSeeFromRight || canSeeFromDown

isTallest :: Int -> [Int] -> Bool
isTallest value others =
    not $ any (>= value) others

at :: Int -> Int -> [[Int]] -> Int
at i j field =
  (field !! i) !! j

