pascall :: [[Int]]
pascall = [(pascallNode x) | x <-[0..]]

pascallNode :: Int -> [Int]
pascallNode n = [(nCr n x) | x <- [0..n]]

nCr :: Int -> Int -> Int
nCr n k = (fact n) `div` ((fact (n-k)) * (fact k))

fact :: Int -> Int
fact 0 = 1
fact n = n * (fact (n-1))
