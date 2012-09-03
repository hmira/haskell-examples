data Bt a = Void
	| Node ( Bt a ) a ( Bt a ) deriving (Show)

kumuluj :: Int -> (Bt a) -> [Bt a]
kumuluj 0 a = [a]
kumuluj _ Void = []
kumuluj 1 (Node l v r)= [ (Node l1 v r) | l1 <- kumuluj 1 l]
		++	[ (Node l v r1) | r1 <- kumuluj 1 r]
		++	[ (Node r v l) ]

kumuluj n (Node l v r)=	[ (Node l1 v r) | l1 <- kumuluj n l]
		++	[ (Node l v r1) | r1 <- kumuluj n r]
		++	[ (Node r1 v l) | r1 <- kumuluj (n-1) r]
		++	[ (Node r v l1) | l1 <- kumuluj (n-1) l]
