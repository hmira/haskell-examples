data Tree a = Void | Node (Tree a) a (Tree a) deriving (Show)

myinsert :: (Ord a) => (Tree a) -> a -> (Tree a)
myinsert (Node left val right) new
	| (val > new) = Node (myinsert left new) val right
	| otherwise = Node left val (myinsert right new)

myinsert Void new = Node Void new Void

addtotreefromlist :: (Ord a) => [a] -> (Tree a) -> (Tree a)
addtotreefromlist [] a = a
addtotreefromlist (a:rest) t = addtotreefromlist rest (myinsert t a)
