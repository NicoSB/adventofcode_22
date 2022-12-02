import System.Environment
import System.IO
import Control.Monad

main :: IO ()
main = do
    args <- getArgs
    result <- do getMaxFood (head args)
    putStrLn (show result)

getMaxFood :: String -> IO Int
getMaxFood filePath = do
    content <- readFile filePath
    let linesOfFiles = lines content
    return(getMaxSum 0 0 linesOfFiles)

getMaxSum :: Int -> Int -> [String] -> Int
getMaxSum current max [] = if current > max then current else max
getMaxSum current max ("":rest) = 
    let newMax = if current > max then current else max
    in getMaxSum 0 newMax rest
    
getMaxSum current max (currentValue:rest) = 
    let x = read currentValue :: Int
        newMax = if current > max then current else max
    in getMaxSum (current + x) newMax rest

    
     

    
