main :: IO ()
main = do
    line <- getLine
    putStrLn (removeDuplicated "" line)

removeDuplicated :: Eq a => [a] -> [a] -> [a]
removeDuplicated xs [] = xs
removeDuplicated xs (y : ys) =
    if y `elem` xs then removeDuplicated xs ys
    else removeDuplicated (xs ++ [y]) ys