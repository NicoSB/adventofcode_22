import Control.Monad
import Data.Char
import Data.List
import Data.Text (splitOn, Text, pack, unpack)
import System.Environment
import System.IO
import qualified Data.Binary.Builder as Text
import Debug.Trace (traceShow, trace)
import GHC.CmmToAsm.AArch64.Instr (stackAlign)

main :: IO ()
main = do
  args <- getArgs
  content <- readFile $ head args
  let result = findMarker [] content 0
  print result


findMarker :: [Char] -> [Char] -> Int -> Int
findMarker [a,b,c,d] _ processed = traceShow [a,b,c,d] processed
findMarker marker (current:rest) processed =
    let maybeIndex = elemIndex current (traceShow marker marker)
    in case maybeIndex of
        Just idx -> findMarker (current:(takeWhile (/= current) marker)) rest processed + 1
        Nothing -> findMarker (current:marker) rest processed + 1
findMarker marker [] processed = -1

  
  
