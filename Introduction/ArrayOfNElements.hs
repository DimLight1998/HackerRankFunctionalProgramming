fn n
    | n == 0 = []
    | otherwise = n : fn (n - 1)