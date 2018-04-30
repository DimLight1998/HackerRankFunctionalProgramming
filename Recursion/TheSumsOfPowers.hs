import           Control.Monad

main :: IO ()
main = do
     args <- map (\ x -> read x :: Integer) <$> (words <$> getLine)
     print $ superDigit (head args) (args !! 1)

superDigit :: Integer -> Integer -> Integer
superDigit n r = superDigitAux (superDigitAux n * r)

superDigitAux :: Integer -> Integer
superDigitAux a
    | a < 10 = a
    | otherwise = superDigitAux $ digitsSum a

digitsSum :: Integer -> Integer
digitsSum a
    | a < 10 = a
    | otherwise = digitsSum (a `div` 10) + (a `mod` 10)
