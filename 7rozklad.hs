primes :: [Int]
primes = 2 : prvocislaOd3

prvocislaOd3 :: [Int]
prvocislaOd3 = filter (isprime primes) [3,5..]

isprime :: [Int] -> Int -> Bool
isprime (a:rest) n 
	| (a * a > n) = True
	| (n `mod` a == 0) = False
	| otherwise = isprime rest n

rozloz :: Int -> [Int]
rozloz n = rozloz1 n primes

rozloz1 :: Int -> [Int] -> [Int]
rozloz1 1 _ = []
rozloz1 n (a:primes) 
	| (n `mod` a == 0) = a : rozloz1 (n `div` a) (a:primes)
	| otherwise = rozloz1 n primes
