{--

a) 
Pro zadanou permutaci čísel 1..N jsme měli 
najít následující v lexikografickém pořadí. 

b) 
Pro dané N a K jsme měli najít K-tou permutaci 
čísel 1..N.

--}


--a)
perm :: [Int] -> [Int]
perm [] = []
perm p = reverse result
	where
		a = last p
		hd = init p
		rp = reverse hd
		result = reorder rp a []

reorder :: [Int] -> Int -> [Int] -> [Int]
reorder (a:rest) val acc 
		| (a > val) = reorder rest val (a:acc)
		| otherwise = (acc ++ [a] ++ [val] ++ rest)


fact :: Int -> Int
fact 0 = 1
fact n = n * fact (n - 1)

--b)
generateperm :: Int -> Int -> [Int]
generateperm k n = result
	where 
		result = generateperm1 k n []

generateperm1 :: Int -> Int -> [Int] -> [Int]
generateperm1 k n list
		| (l1 == n) = list
		| otherwise = generateperm1 rest n (list ++ [mem])
	where
		a = fact ((n-l1)-1)
		rest = k `mod` a
		coef = k `div` a
		mlist = [x | x <- [1..n], notElem x list]
		mem = mlist !! coef
		l1 = length list












