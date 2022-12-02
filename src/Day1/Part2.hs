import System.Environment
import System.IO
import Control.Monad

main :: IO ()
main = do
    args <- getArgs
    result <- do getMaxFood (head args)
    putStrLn (show (sum result))

getMaxFood :: String -> IO [Int]
getMaxFood filePath = do
    content <- readFile filePath
    let linesOfFiles = lines content
    return(getMaxSum 0 [] linesOfFiles)

getMaxSum :: Int -> [Int] -> [String] -> [Int]
getMaxSum current stack [] = insert current stack
getMaxSum current stack ("":rest) = 
    getMaxSum 0 (insert current stack) rest
getMaxSum current stack (currentValue:rest) = 
    let x = read currentValue :: Int
    in getMaxSum (current + x) stack rest

insert :: Int -> [Int] -> [Int]
insert val [] = [val]
insert val (a:[]) = if val > a then [val, a] else [a, val]
insert val (a:b:[]) = if val > a then [val, a, b] else if val > b then [a, val, b] else [a, b, val]
insert val (a:b:c:[]) = if val > a then [val, a, b] else if val > b then [a, val, b] else if val > c then [a, b, val] else [a, b, c]


    
