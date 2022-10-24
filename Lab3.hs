wordList :: String->[String]
wordList s = y where y=nullCheck(findWords s)

latinCheck :: Char -> Bool
latinCheck s | s>='A'&&s<='Z' = True
			 | s>='a'&&s<='z' = True
			 | otherwise = False

nullCheck :: [String] -> [String]
nullCheck [] = []
nullCheck (h:t)
			| h == ""
				= nullCheck t
			|otherwise
				= h : nullCheck t

findWords :: String -> [String]
findWords [] = [""]
findWords (h:t) 
			| latinCheck h = (h : head rest) : tail rest
			| otherwise = "" : rest
    			where rest = findWords t