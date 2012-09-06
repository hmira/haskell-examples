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
