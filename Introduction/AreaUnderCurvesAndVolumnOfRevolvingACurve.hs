import Text.Printf (printf)
import Numeric

powerPair :: Double -> (Int, Int) -> Double
powerPair base cn = fromIntegral (fst cn) * base ^^ snd cn

valueAt :: [Int] -> [Int] -> Double -> Double
valueAt coff expo val = sum [powerPair val coe | coe <- zip coff expo]

integrate :: [Int] -> [Int] -> Int -> Int -> Double
integrate coff expo left right = sum [0.0005 * (valueAt coff expo x + valueAt coff expo (x + 0.001)) | x <- [fromIntegral left, fromIntegral left + 0.001 .. fromIntegral right]]

getSquare :: [Int] -> [Int] -> ([Int], [Int])
getSquare coff expo = ([getCoff n coff expo | n <- [minExpo .. maxExpo]], [minExpo .. maxExpo]) where
    maxExpo = maximum expo * 2
    minExpo = minimum expo * 2
    getCoff n coff expo = sum [(coff !! x) * (coff !! y) | x <- lenList, y <- lenList, (expo !! x) + (expo !! y) == n] where
        lenList = if null coff then [] else [0 .. length coff - 1]

-- This function should return a list [area, volume].
solve :: Int -> Int -> [Int] -> [Int] -> [Double]
solve l r a b = [integrate a b l r, pi * uncurry integrate (getSquare a b) l r]

--Input/Output.
main :: IO ()
main = getContents >>= mapM_ (printf "%.1f\n"). (\[a, b, [l, r]] -> solve l r a b). map (map read. words). lines
