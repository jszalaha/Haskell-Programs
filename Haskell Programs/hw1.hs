---------------------------
--	Program: hw1.hs
--	Author: Jordan Zalaha
--	email: jszalaha@ucsc.edu
--	Date: January 17, 2016
---------------------------

-- 1.
citeAuthor :: String -> String -> String
citeAuthor first last = last ++ ", " ++ first

-- 2.
initials :: String -> String -> String
initials first last = (head first):"." ++ (head last):"."

-- 3.
title :: (String, String, Int) -> String
title (_, x, _) = x 

-- 4.
citeBook :: (String, String, Int) -> String
citeBook (author, title, year) =  title ++ " (" ++ author ++ ", " ++ (show year) ++ ")"

-- 5.
bibliography_rec :: [(String, String, Int)] -> String
bibliography_rec [] = ""
bibliography_rec (first:rest) = citeBook first ++ "\n" ++ bibliography_rec rest

-- 6.
-- Helper function used to produce a list of only the
-- year field in each tuple.
years :: [(String, String, Int)] -> [Int]
years [] = []
years ((_, _, x):rest) = x:(years rest)

averageYear :: [(String, String, Int)] -> Int
averageYear input = sum (years input) `div` (length input)

-- 7. & 8.
txt :: String
txt = "[1] and [2] both feature characters who will do whatever it takes to " ++
      "get to their goal, and in the end the thing they want the most ends " ++
      "up destroying them.  In case of [2] this is a whale..."

-- Helper function used to check if a given string
-- is in the reference format "[#]"	  
isReference :: String -> Bool
isReference [] = True
isReference (x:xs)
	| x `elem` ['[']				= isReference xs
	| x `elem` ['0'..'9']			= isReference xs
	| x `elem` [']'] && xs == []	= True
	| otherwise						= False

-- Helper function used to read the reference digit
refNumber :: String -> Int
refNumber [] = 0
refNumber (x:xs)
	| x `elem` ['[']				= refNumber xs
	| x `elem` ['0'..'9']			= read (x:[])
	| otherwise						= 0
	
references :: String -> Int
references str = length (filter isReference (words str))

-- Helper function used to replace a given string that
-- fits the reference format with its corresponding 
-- book from the list
refReplace :: [(String, String, Int)] -> String -> String
refReplace books ref
	| isReference ref		= citeBook (books!!((refNumber ref)-1))
	| otherwise				= ref

citeText :: [(String, String, Int)] -> String -> String
citeText books text = unwords (map (refReplace books) (words text)) 