import Data.Char

-- Lowercases all the characters in a string.
myToLower :: String -> String
myToLower []  	   = []
myToLower (x:xs)   = (Data.Char.toLower x) : myToLower xs
	  where toLower = Data.Char.toLower


-- Capitalizes first characters of words; lowercases the rest.
myCapFirstLetters :: String -> String
myCapFirstLetters [] = []
myCapFirstLetters fullString@(x:xs)
		  | isAlphaNum x = (myCapWord . fst . splitFirstWord $ fullString) 
		    	           ++ (myCapFirstLetters . snd . splitFirstWord $ fullString)
		  | otherwise  	 = x : (myCapFirstLetters xs)

-- From a string, splits off the starting contiguous chunk of alphanumerics.
-- Returns a list containing the alphanumeric chunk and the rest.
splitFirstWord :: String -> (String, String)
splitFirstWord [] 	 = ([], [])
splitFirstWord ls	 = splitter ls []
	       where splitter (x:xs) firstChunk
	       	     	| isAplhaNum x = splitter xs (x : firstChunk)
			| otherwise    = (reverse firstChunk, x:xs)
			where isAplhaNum = Data.Char.isAlphaNum

-- Takes a single word, capitalizes the first letter, and lowercases the rest.
myCapWord :: String -> String 
myCapWord (x:xs)   = (toUpper x) : myToLower xs
	  where toUpper = Data.Char.toUpper