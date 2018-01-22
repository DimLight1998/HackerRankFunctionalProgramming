main :: IO ()
main = do
    numString <- getLine
    let num = read numString :: Int
    act num

act :: Int -> IO ()
act 0 = return ()
act x = do
    line <- getLine
    putStrLn (change line)
    act (x - 1)

change :: String -> String
change [] = []
change (a : b : xs) = b : a : change xs
