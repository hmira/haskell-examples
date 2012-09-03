maxlex :: Int -> [[Int]]
maxlex n = gen n 0

gen :: Int -> Int -> [[Int]]
gen m n = (genset m n) ++  gen m (n + 1)

genset :: Int -> Int -> [[Int]]
genset 0 _ = []
genset m n = filter (elem n) (multiplelist m n)

multiplelist :: Int -> Int -> [[Int]]
multiplelist 0 _ = [[]]
multiplelist m n = [ (x:xs)| x <- [0..n], xs <- (multiplelist (m-1) n)]
