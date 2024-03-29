import           Data.Vector
import           Prelude     hiding (map, mapM_, maximum)

buildVec :: Int -> Vector Int
buildVec n = v where
    v = generate (n + 1) f
    f 0 = 0
    f 1 = 1
    f x = (v ! (x - 1)) + 3 * x - 2

main :: IO ()
main = do
    t <- (\x -> read x :: Int) <$> getLine
    ns <- replicateM t getLine
    let tests = map (\x -> read x :: Int) ns
    let v = buildVec $ maximum tests
    mapM_ (\x -> print $ v ! x) tests
    return ()
