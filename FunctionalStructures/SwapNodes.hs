data Tree = Leaf | Node Tree Int Tree deriving Show

build :: Int -> Tree
build (-1) = Leaf
build x = Node Leaf x Leaf

addToTree :: Int -> Int -> Int -> Tree -> Tree
addToTree _ _ _ Leaf = Leaf
addToTree index left right (Node l x r)
    | x == index = Node (build left) x (build right)
    | otherwise = Node (addToTree index left right l) x (addToTree index left right r)

builds :: [(Int, Int)] -> Tree
builds xs = builds' xs (build 1) 1 where
    builds' [] t _ = t
    builds' (x : xs) t curr = builds' xs (uncurry (addToTree curr) x t) (curr + 1)

depth :: Tree -> Int
depth Leaf = 0
depth (Node l x r) = max (depth l) (depth r) + 1

toInOrderArray :: Tree -> [Int]
toInOrderArray Leaf = []
toInOrderArray (Node l x r) = toInOrderArray l ++ [x] ++ toInOrderArray r

swapAt :: Int -> Tree -> Tree
swapAt _ Leaf = Leaf
swapAt d (Node l x r) = swapAt' d d (Node l x r) where
    swapAt' _ _ Leaf = Leaf
    swapAt' d c (Node l x r)
        | c == 1 = Node (swapAt' d d r) x (swapAt' d d l)
        | otherwise = Node (swapAt' d (c - 1) l) x (swapAt' d (c - 1) r)

swaps :: [Int] -> Tree -> [[Int]]
swaps [] _ = []
swaps (x : xs) t = toInOrderArray t' : swaps xs t' where t' = swapAt x t

ints2Str :: [Int] -> String
ints2Str [] = ""
ints2Str [x] = show x
ints2Str (x : xs) = show x ++ " " ++ ints2Str xs

format :: [String] -> [(Int, Int)]
format [] = []
format (x : xs) = format' x : format xs where
    format' x = (read (head (words x)) :: Int, read (last (words x)) :: Int)

readLines :: Int -> IO [String]
readLines 0 = return []
readLines n = do
    line <- getLine
    rest <- readLines (n - 1)
    return (line : rest)

putStrLns :: [String] -> IO ()
putStrLns [] = return ()
putStrLns (x : xs) = do
    putStrLn x
    putStrLns xs

str2Int :: String -> Int
str2Int s = read s :: Int

main :: IO ()
main = do
    numNodeStr <- getLine
    let numNode = read numNodeStr :: Int
    nodeStrs <- readLines numNode
    let tree = builds $ format nodeStrs
    numSwapStr <- getLine
    let numSwap = read numSwapStr :: Int
    swapStrs <- readLines numSwap
    let swapss = map str2Int swapStrs
    let ans = swaps swapss tree
    putStrLns $ map ints2Str ans