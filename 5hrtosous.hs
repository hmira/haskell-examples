hrsous :: (Eq a) => (Num b) => [(a, a, b)] -> [(a, [(a, b)])]
hrsous g = hrsous1 g []

hrsous1 :: (Eq a) => (Num b) => [(a, a, b)] -> [(a, [(a, b)])] -> [(a, [(a, b)])]
hrsous1 [] accumulated = accumulated
hrsous1 (a:rest) accumulated = hrsous1 rest (addtoacc a accumulated)

addtoacc :: (Eq a) => (Num b) => (a, a, b) -> [(a, [(a, b)])] -> [(a, [(a, b)])]
addtoacc (a1, a2, rank) [] = [(a1, [(a2, rank)])]
addtoacc (a1, a2, rank) ((f1, flist) : accumulated)
	| (a1 == f1) = ((f1, flist ++ [(a2, rank)]) : accumulated)
	| otherwise = (f1, flist) : addtoacc (a1, a2, rank) accumulated
