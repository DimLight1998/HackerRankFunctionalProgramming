import Control.Applicative
import Control.Monad
import System.IO

helloWorld n = case n of
    1 -> putStrLn "Hello World"
    n -> do
        putStrLn "Hello World"
        helloWorld (n - 1)

main :: IO ()
main = do
    n_temp <- getLine
    let n = read n_temp :: Int
    --  Print "Hello World" on a new line 'n' times.
    helloWorld n
    

getMultipleLines :: Int -> IO [String]
getMultipleLines n
    | n <= 0 = return []
    | otherwise = do          
        x <- getLine         
        xs <- getMultipleLines (n-1)    
        let ret = (x:xs)    
        return ret          

