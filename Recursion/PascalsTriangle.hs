getLineStrs :: Int -> String
getLineStrs x = strsToStr $ map (listToStr . pascalTriangle) [1 .. x]

pascalTriangle :: Int -> [Int]
pascalTriangle 1 = [1]
pascalTriangle n = [1] ++ produce (pascalTriangle (n - 1)) ++ [1] where
    produce []     = []
    produce [x]    = []
    produce xs     = [xs !! n + xs !! (n + 1) | n <- [0 .. length xs - 2]]

listToStr :: [Int] -> String
listToStr = foldr ((\ x y -> x ++ " " ++ y) . show) ""

strsToStr :: [String] -> String
strsToStr = foldr (\ x y -> x ++ "\n" ++ y) ""

main = do
    input <- getLine
    let m = read input :: Int
    putStr $ getLineStrs m