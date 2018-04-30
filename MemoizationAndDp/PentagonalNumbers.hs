import Control.Monad

pentangonalNums :: Int -> [Int]
pentangonalNums 1 = [1]
pentangonalNums x = (3 * x - 2+ head pre) : pre where
    pre = pentangonalNums $ x - 1

strToInt :: String -> Int
strToInt s = read s :: Int

main :: IO ()
main = do
    numNum <- getLine
    let num = read numNum :: Int
    lines <- replicateM num getLine
    let nums = map strToInt lines
    let pNums = pentangonalNums $ maximum nums
    mapM_ print [pNums !! (maximum nums - n) | n <- nums]
    return ()