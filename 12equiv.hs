{--

equivalence [('a','b'),('b','c'),('d','e')]

[
('a','b'),('b','c'),('d','e'),('a','c'),
('a','c'),('b','a'),('c','b'),('e','d'),
('c','a'),('a','a'),('b','b'),('d','d'),
('c','c'),('e','e')]

--}

equivalence :: (Eq a) => [(a,a)] -> [(a,a)]
equivalence list = result
	where
		tresult = transitivity list list
		sresult = symmetry tresult tresult
		rresult = reflexivity sresult sresult
		result = rresult

symmetry :: (Eq a) => [(a,a)] -> [(a,a)] -> [(a,a)]
symmetry [] result = result
symmetry ((a,b):rest) acc 
			| (elem (b,a) acc) = symmetry rest acc
			| otherwise = symmetry rest (acc ++ [(b,a)])

reflexivity :: (Eq a) => [(a,a)] -> [(a,a)] -> [(a,a)]
reflexivity [] result = result
reflexivity ((a,_):rest) acc
			| (elem (a,a) acc) = reflexivity rest acc
			| otherwise = reflexivity rest (acc ++ [(a,a)])

transitivity :: (Eq a) => [(a,a)] -> [(a,a)] -> [(a,a)]
transitivity [] result = result
transitivity ((a,b):rest) acc
	| (a==b) = transitivity rest acc
	| otherwise = (transitivity rest acc) ++ added
	where
		added1 = [(a,c) | (b1,c) <- acc, notElem (a,c) acc, b1 == b, a/=c]
		added2 = added1 ++ [(c,a) | (c,b1) <- acc, notElem (c,a) (acc ++ added1), b1 == b, a/=c]
		added3 = added2 ++ [(b,c) | (a1,c) <- acc, notElem (b,c) (acc ++ added2), a1 == a, b/=c]
		added4 = added3 ++ [(c,b) | (c,a1) <- acc, notElem (c,b) (acc ++ added3), a1 == a, b/=c]
		added = added4
