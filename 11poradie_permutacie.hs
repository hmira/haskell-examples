{--

Na vstupu máte permutaci délky N, spočtěte její lexikografické pořadí.

--}

fact :: Int -> Int
fact 0 = 1
fact n = n * fact (n-1)

permutation_order :: [Int] -> Int
permutation_order [] = 1 -- aby 0. bolo 1.
permutation_order (p:perm) = 
	let
		decr_perm = [ if (x > p) then (x-1) else (x) | x <- perm]
		a = (p-1) * b
		b = fact (length perm)
	in
		a + (permutation_order decr_perm)
