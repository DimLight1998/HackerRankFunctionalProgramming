import Control.Applicative
import Control.Monad
import System.IO

factory :: Int -> Int
factory n = if n == 0 then 1 else product [1 .. n]

power :: Double -> Int -> Double
power a b = if b == 0 then 1 else foldl (\ x y -> x * a) 1 [1 .. b]

ex :: Double -> Double
ex d = sum [power d n / fromIntegral $ factory n | n <- [0 .. 9]]

main :: IO ()
main = do
    n_temp <- getLine
    let n = read n_temp :: Int
    forM_ [1..n] $ \a0  -> do
        x_temp <- getLine
        let x = read x_temp :: Double
        print (ex x)


getMultipleLines :: Int -> IO [String]
getMultipleLines n
    | n <= 0 = return []
    | otherwise = do          
        x <- getLine         
        xs <- getMultipleLines (n-1)    
        let ret = (x:xs)    
        return ret          

