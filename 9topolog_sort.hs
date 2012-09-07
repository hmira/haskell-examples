{-- 

topologický sort grafu

--}

data Vertex a = Vertex a [a] deriving Show
data Graph a = Vertices [Vertex a]

topSort :: (Eq a) => (Graph a) -> [a]
topSort (Vertices []) = []
topSort (Vertices list) = (a:(topSort reducedList))
	where
		a = findLast list
		reducedList1 = removeEdge list a
		reducedList2 = removeVertex reducedList1 a
		reducedList = (Vertices reducedList2)

findLast :: [Vertex a] -> a
findLast [] = error "cycle"
findLast ((Vertex a []):xs) = a
findLast (x:xs) = findLast xs

removeVertex :: (Eq a) => [Vertex a] -> a -> [Vertex a]
removeVertex list a = filter (cmpvertex a) list

removeEdge :: (Eq a) => [Vertex a] -> a -> [Vertex a]
removeEdge [] _ = []
removeEdge ((Vertex v list):xs) a = (Vertex v (filter (/=a) list) : (removeEdge xs a))

cmpvertex :: (Eq a) => a -> (Vertex a) -> Bool
cmpvertex a (Vertex b _) = (b /= a)
