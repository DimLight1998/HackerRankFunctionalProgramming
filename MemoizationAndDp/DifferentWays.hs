import           Control.Monad
import           Data.Vector   hiding (head, map, mapM_, maximum, replicateM)
import           Prelude       hiding (replicate)

modulo :: Int
modulo = 100000007

pascalTriangle :: Int -> Int -> Vector (Vector Int)
pascalTriangle m n = v where
    v = generate (m + 1) f
    f 0 = replicate (n + 1) 0
    f x = generate (n + 1) (g x)
    g x y
        | y == 0 || y == x = 1
        | otherwise = (v ! (x - 1) ! (y - 1) + v ! (x - 1) ! y) `mod` modulo

main :: IO ()
main = do
    t <- (\x -> read x :: Int) <$> getLine
    ns <- replicateM t getLine
    let tests = map (\x -> let w = words x in (read (head w) :: Int, read (w !! 1) :: Int)) ns
    let maxM = maximum [fst p | p <- tests]
    let maxN = maximum [snd p | p <- tests]
    let vec = pascalTriangle maxM maxN
    mapM_ (\(x, y) -> print (vec ! x ! y)) tests
    return ()
