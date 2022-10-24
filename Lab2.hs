search :: Integer->Integer->Integer->Integer
search a k m = recur 1 a k m

recur :: Integer->Integer->Integer->Integer->Integer
recur n a k m
	| (n+a)^k>m^n
		= recur(n+1) a k m
	| otherwise
		= n

compress :: Integer -> Integer
compress n
	| p <= 9
		= p
	| otherwise
		= compress(p)
	where p = mulNums(n)

mulNums :: Integer -> Integer
mulNums n
	| n <= 9
		= n
	| d == 0
		= mulNums((n `div` 10) ) * 1
	| otherwise
		= mulNums((n `div` 10) ) * d
	where d = (n `mod` 10)



