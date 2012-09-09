{-- 

Definovat funkci, ktera overi, ze zadana posloupnost je Grayuv kod.
Coz je posloupnost binarnich cisel "stejne delky",
kde sousedni cisla se lisi maximalne o jeden bit,
vcetne prvniho a posledniho cisla a jejich pocet je 2^n.

--}

isGray::[[Int]] -> Bool
isGray (a:list)
	| (differ a b 1 && l == 2^n) = isGrayCode (a:list)
	| otherwise = False
	where
		b = last list
		l = length (a:list)
		n = length a

isGray [] = True

isGrayCode:: [[Int]] -> Bool
isGrayCode (a:[]) = True
isGrayCode (a1:a2:a) 
	| (differ a1 a2 1 && notElem a1 (a2:a)) = isGrayCode (a2:a)
	| otherwise = False

differ:: [Int] -> [Int] -> Int -> Bool
differ (a1:a) (b1:b) 1
	| (a1 == b1) = differ a b 1
	| otherwise = differ a b 0

differ (a1:a) (b1:b) 0
	| (a1 == b1) = differ a b 0
	| otherwise = False

differ [] [] 0 = True
differ _ _ _ = False
