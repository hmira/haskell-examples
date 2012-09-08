{--
Mali sme zadaný graf (orientovaný, bez smyčiek, bez izolovaných bodov a ešte niečo..) ako zoznam hrán s ohodnoteniami a bolo ho treba previesť na zoznam vrcholov, kde ku každému vrcholu prislúchal zoznam susedov s ohodnotením.
Deklarácia mohla vyzerať takto:
hr2sous :: (Eq a) => (Num b) => [(a,a,b)] -> [(a,[(a,b)])]

Príklad:
hr2sous [ ('a','b','1'), ('b','a',2), ('b','c',3), ('c','d',4) ]
[ ('a', [('b',1)]), ('b', [('a',2), ('c',3)]), ('c', [('d',4)]) ]

--}

hr2sous :: (Eq a) => (Num b) => [(a,a,b)] -> [(a, [(a,b)])]
hr2sous [] = []
hr2sous ((vertex, adj, value):rest) = ((vertex, ((adj,value):edgelist)):(hr2sous rest))
	where edgelist = [ (vb, c) | (vx, vb, c) <- rest, vx == vertex ]
