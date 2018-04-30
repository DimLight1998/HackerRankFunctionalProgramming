import Control.Monad

isValidBst :: [Int] -> Bool
isValidBst [] = True
isValidBst (x : xs)
    | lxs ++ rxs == xs && all (> x) rxs = isValidBst lxs && isValidBst rxs
    | otherwise = False
    where lxs = takeWhile (< x) xs; rxs = dropWhile (<= x) xs

getEvenLine :: IO String
getEvenLine = do
    getLine
    getLine

readInt :: String -> Int
readInt s = read s :: Int

main :: IO ()
main = do
    num <- read <$> getLine :: IO Int
    querys <- replicateM num getEvenLine
    mapM_ ((\ x -> putStrLn (if x then "YES" else "NO")) . isValidBst . map readInt . words) querys
