main :: IO ()
main = do
    line <- getLine
    putStrLn (compress line)

compress :: String -> String
compress "" = ""
compress (x : xs) = compressWith x 1 xs

compressWith :: Char -> Int -> String -> String
compressWith c x "" = c : (if x == 1 then "" else show x)
compressWith c x (ch : cs) = 
    if c == ch then compressWith c (x + 1) cs
    else c : (if x == 1 then "" else show x) ++ compress (ch : cs)