{--

Vymyslet strukturu binarniho stromu, ktery v listech uchovava jeden typ hodnot a ve vrcholech druhy typ hodnot. Substituovat za typ v listech a napsat funkci, ktera ocisluje listy ve stromu od 1 zprava.

marktree (Node (Node (Leaf 0) 'a' (Leaf 0)) 'b' (Node (Leaf 0) 'c' (Leaf 0)))

Node (Node (Leaf 3) 'a' (Leaf 2)) 'b' (Node (Leaf 1) 'c' (Leaf 0))

--}


data BinTree a b = Leaf a | Node (BinTree a b) b (BinTree a b) deriving (Show, Eq)

marktree :: (BinTree Int b) -> (BinTree Int b)
marktree t = newtree
	where
		(newtree, _) = markleaves 0 t

markleaves :: Int -> (BinTree Int b) -> ((BinTree Int b), Int)
markleaves count (Leaf _) = ((Leaf count), count + 1)
markleaves count (Node l v r) = (tree, newcount)
	where
		(r1, count1) = markleaves count r
		(l1, count2) = markleaves count1 l
		tree = Node l1 v r1
		newcount = count2
