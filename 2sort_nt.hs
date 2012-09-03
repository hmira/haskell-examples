import Data.List

data NTree a = NTree a [NTree a] deriving (Show)

applyFAll :: (a->a) -> [a] -> [a]
applyFAll _ [] = []
applyFAll f (a:rest) = (f a) : (applyFAll f rest)

inc5 :: Int -> Int
inc5 i = 5 + i

sortNT :: (k->k->Bool) -> (NTree (k, h)) -> (NTree (k, h))
sortNT _ (NTree (key, value) []) = (NTree (key, value) [])
sortNT cmpf (NTree (key, value) list) = 
	NTree (key, value) rlist
		where rlist = sortBy myf alist
			where 
				alist = applyFAll (sortNT cmpf) list
				myf (NTree (k1,h1) l1) (NTree (k2,h2) l2) 
					| cmpf k1 k2 = LT
					| otherwise = GT
