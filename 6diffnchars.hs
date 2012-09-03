abeceda :: [Char]
abeceda = "ab"

diffs :: [Char] -> Int -> [[Char]]
diffs string 0 = diffnchars string 0
diffs string n = diffnchars string n ++ diffs string (n-1)

diffnchars :: [Char] -> Int -> [[Char]]
diffnchars string 0 = [string]
diffnchars [] _ = []
diffnchars (a:string) n =
	[ a:nstring | nstring <- diffnchars string (n) ] ++
	[ c:n1string | c<-abeceda, n1string<- diffnchars string (n-1)]
