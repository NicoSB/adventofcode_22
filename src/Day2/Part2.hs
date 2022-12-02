import System.Environment
import System.IO
import Control.Monad


main :: IO ()
main = do
    args <- getArgs
    lines <- do getLines (head args)
    let result = sum(map getMatchScore lines)
    putStrLn (show result)

getLines :: String -> IO [String]
getLines filePath = do
    content <- readFile filePath
    return(lines content)

getMatchScore :: String -> Int
getMatchScore line =
    let lineWords = words line
        theirs = head (lineWords!!0)
        desiredResult = head (lineWords!!1)
        ours = getOurPick theirs (desiredResult)
        shapeScore = getShapeScore ours
        resultScore = getResultScore theirs ours
    in shapeScore + resultScore

getOurPick :: Char -> Char -> Char
getOurPick 'A' 'X' = 'Z'
getOurPick 'A' 'Y' = 'X'
getOurPick 'A' 'Z' = 'Y'
getOurPick 'B' 'X' = 'X'
getOurPick 'B' 'Y' = 'Y'
getOurPick 'B' 'Z' = 'Z'
getOurPick 'C' 'X' = 'Y'
getOurPick 'C' 'Y' = 'Z'
getOurPick 'C' 'Z' = 'X'

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

    
     

    
