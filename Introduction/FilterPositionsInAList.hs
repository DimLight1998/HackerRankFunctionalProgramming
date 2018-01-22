f :: [Int] -> [Int]
f lst
    | length lst < 2 = []
    | otherwise = (lst !! 1) : f (drop 2 lst)

-- This part deals with the Input and Output and can be used as it is. Do not modify it.
main = do
   inputdata <- getContents
   mapM_ (putStrLn. show). f. map read. lines $ inputdata
