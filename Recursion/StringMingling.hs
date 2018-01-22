mingling :: String -> String -> String
mingling [] _ = []
mingling p q  = [head p] ++ [head q] ++ mingling (tail p) (tail q)

main = do
    p <- getLine
    q <- getLine
    putStrLn $ mingling p q