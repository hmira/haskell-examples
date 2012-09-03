data Tree a = Node a [Tree a]
data BTree a = BNode (BTree a) a (BTree a) | BVoid deriving (Show)

conv :: (Tree a) -> (BTree a)
conv (Node x []) = BNode BVoid x BVoid
conv (Node x [a]) = BNode left x BVoid
	where
	left = conv a
conv (Node x (a1:(Node x2 trees2):trees) ) = BNode left x right
	where 
	right = conv (Node x2 (trees2 ++ trees))
	left = conv a1
