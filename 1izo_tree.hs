{--
Napište funkci izo i :: Int -> Bt a -> [Bta a],
 která dostane číslo N a strom S a vyrobí 
seznam všech stromů, které mají v právě N vrcholech 
prohozenou pravou a levou větev.
 --}

data Bt a = Void
	| Node ( Bt a ) a ( Bt a ) deriving (Show)

izo :: Int -> (Bt a) -> [Bt a]
izo 0 a = [a]
izo 1 (Node Void _ Void) = []
izo _ Void = []
izo n (Node l v r)=
	concat	[
		[ (Node l1 v r1) | l1 <- izo (n-m) l, r1 <- izo m r]
		| m <- [0..n]
		]
	++
	concat	[
		[ (Node r1 v l1) | l1 <- izo ((n-1)-m) l, r1 <- izo m r]
		| m <- [0..(n-1)]
		]

{-- 

izo 1 (Node (Node (Node Void 1 Void) 2 (Node Void 3 Void)) 4 (Node Void 6 Void))

Node (Node (Node Void 3 Void) 2 (Node Void 1 Void)) 4 (Node (Node Void 5 Void) 6 (Node Void 7 Void))
Node (Node (Node Void 1 Void) 2 (Node Void 3 Void)) 4 (Node (Node Void 7 Void) 6 (Node Void 5 Void))
Node (Node (Node Void 5 Void) 6 (Node Void 7 Void)) 4 (Node (Node Void 1 Void) 2 (Node Void 3 Void))

--}
