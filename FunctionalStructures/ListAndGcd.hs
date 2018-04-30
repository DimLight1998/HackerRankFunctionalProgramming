import Control.Monad
import Data.List

listGcd :: [(Int, Int)] -> [(Int, Int)] -> [(Int, Int)]
listGcd _ [] = []
listGcd [] _ = []
listGcd (x : xs) (y : ys)
    | fst x < fst y = listGcd xs (y : ys)
    | fst x > fst y = listGcd (x : xs) ys
    | otherwise = (fst x, min (snd x) (snd y)) : listGcd xs ys

format :: String -> [(Int, Int)]
format s = format' $ words s where
    format' [] = []
    format' (x : y : xs) = (read x :: Int, read y :: Int) : format' xs

unformat :: [(Int, Int)] -> String
unformat xs = unwords (foldr (\ x y -> [show $ fst x, show $ snd x] ++ y) [] xs)

main :: IO ()
main = do
    numStr <- getLine
    lines <- replicateM (read numStr :: Int) getLine
    putStr $ unformat $ foldl1 listGcd (map format lines)
