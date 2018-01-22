main :: IO ()
main = do
    xs <- getLine
    ys <- getLine
    let n = getCommonPrefixLength xs ys
    putStrLn (show n ++ " " ++ take n xs)
    putStrLn (show (length xs - n) ++ " " ++ drop n xs)
    putStrLn (show (length ys - n) ++ " " ++ drop n ys)

getCommonPrefixLength :: String -> String -> Int
getCommonPrefixLength "" _ = 0
getCommonPrefixLength _ "" = 0
getCommonPrefixLength (x : xs) (y : ys) =
    if x == y then 1 + getCommonPrefixLength xs ys
    else 0

