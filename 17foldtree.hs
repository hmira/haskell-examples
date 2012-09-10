{-- 

pre štruktúru Tree a vyrobiť ekvivalent
foldr / foldl

--}


data Tree a = Void | Node (Tree a) a (Tree a)

fold :: (a -> b -> b) -> b -> (Tree a) -> b
fold _ a Void = a
fold op a (Node left val right) = op val sum
	where
		a1 = fold op a left
		sum = fold op a1 right
