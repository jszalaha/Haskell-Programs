----------------------------------------------------------
-- Program: hw2.hs
-- Author: Jordan Zalaha
-- Email: jszalaha@hotmail.com
-- Date: January 24, 2016
----------------------------------------------------------

-- 1.
myFoldl :: (a -> b -> a) -> a -> [b] -> a
myFoldl f z [] = z
myFoldl f z (x:xs) = myFoldl f (f z x) xs

-- 2.
myReverse :: [a] -> [a]
myReverse = foldl (\acc x -> x : acc) []

-- 3.
myFoldr :: (a -> b -> b) -> b -> [a] -> b
myFoldr f z xs = foldl (\g x y -> g (f x y)) id xs z

-- 4.
myFoldl2 :: (a -> b -> a) -> a -> [b] -> a
myFoldl2 f z xs = foldr (\x g y -> g (f y x)) id xs z

-- 5.
isUpper :: Char -> Bool
isUpper x = if x `elem` ['A'..'Z']
            then True
            else False

-- 6.			
onlyCapitals1 :: String -> String
onlyCapitals1 = filter isUpper

-- 7.					   
onlyCapitals2 :: String -> String
onlyCapitals2 xs = [ x | x <- xs, isUpper x ]

-- 8.
onlyCapitals3 :: String -> String
onlyCapitals3 [] = []
onlyCapitals3 (x:xs)
    | isUpper x        = x:(onlyCapitals3 xs)
	| otherwise        = onlyCapitals3 xs

-- 9.	
divRemainder :: Int -> Int -> (Int, Int)
divRemainder numer denom = 
	let	quotient = numer `div` denom 
		remainder = (numer `mod` denom)
	in	(quotient, remainder)
	
-- 10.
digitSum :: Int -> Int
digitSum x 
	| ones /= 0 && rest /= 0	= ones + (digitSum rest)
	| rest /= 0					= digitSum rest
	| otherwise					= x
    where ones = x `mod` 10
          rest = x `div` 10

--------------------------------------------------------------------------
-- 11.
--------------------------------------------------------------------------
-- Helper Functions --

convertToList :: Int -> [Int]
convertToList 0 = []
convertToList x = first:(convertToList rest)
    where first = x `mod` 10
          rest = x `div` 10

sectionList :: [Int] -> [[Int]]
sectionList [] = []
sectionList xs = (take 3 xs):(sectionList (drop 3 xs))

digits :: [String]
digits = ["zero", "one", "two", "three", "four", "five", "six", "seven", "eight", "nine"]
teens :: [String]
teens = ["", "eleven", "twelve", "thirteen", "fourteen", "fifteen", "sixteen", "seventeen", "eighteen", "nineteen"]
tens :: [String]
tens = ["", "ten", "twenty", "thirty", "forty", "fifty", "sixty", "seventy", "eighty", "ninety"]
	
say :: Int -> String -> String
say x str
	| str == "digit"	= digits!!x
	| str == "teens"	= teens!!x
	| str == "tens"		= tens!!x

sayGroup :: [Int] -> String
sayGroup (x:xs)
	| threeDigit && (last xs) == 0 && (head xs) == 0			= xDigit
	| threeDigit && (last xs) == 0 && x == 0					= xTen
	| threeDigit && (head xs) == 0 && x == 0					= xHundred
	| threeDigit && (last xs) == 0 && (head xs) /= 1 && x /= 0	= xTen ++ " " ++ xDigit
	| threeDigit && (last xs) == 0 && (head xs) == 1 && x /= 0	= xTeen
	| threeDigit && (head xs) == 1 && x /= 0					= xHundred ++ " " ++ xTeen
	| threeDigit && x == 0										= xHundred ++ " " ++ xTen
	
	| twoDigit && (head xs) == 0								= xDigit
	| twoDigit &&  x == 0										= xTen
	| twoDigit && (head xs) /= 1 && x /= 0						= xTen ++ " " ++ xDigit
	| twoDigit && (head xs) == 1 && x /= 0						= xTeen
	
	| xs == []													= xDigit
	
	| otherwise													= xHundred ++ " " ++ xTen ++ " " ++ xDigit
    where xHundred = (say (last xs) "digit") ++ " hundred"
          xDigit = say x "digit"
          xTen = say (head xs) "tens"
          xTeen = say x "teens"
          threeDigit = (length xs) == 2
          twoDigit = (length xs) == 1

sayList :: Int -> [String] -> String
sayList n [] = ""
sayList n (x:xs) 
	| n == 0 && z		= sayList (n+1) xs
	| n == 0			= sayList (n+1) xs ++ x
	| n == 1 && z		= rest
	| n == 1			= rest ++ " thousand "
	| n == 2 && z		= rest
	| n == 2			= rest ++ " million "
	| n == 3 && z		= rest
	| n == 3			= rest ++ " billion "
	| n == 4 && z		= rest
	| n == 4			= rest ++ " trillion "
	| n == 5 && z		= rest	
	| n == 5			= rest ++ " quadrillion "
	| n == 6 && z		= rest
	| n == 6			= rest ++ " quintillion "
	| n == 7 && z		= rest
	| n == 7			= rest ++ " sextillion "
	| n == 8 && z		= rest
	| n == 8			= rest ++ " septillion "
	| n == 9 && z		= rest
	| n == 9			= rest ++ " octillion "
	| n == 10 && z		= rest
	| n == 10			= rest ++ " nonillion "
	| n == 11 && z		= rest
	| n == 11			= rest ++ " decillion "
	| n == 12 && z		= rest
	| n == 12			= rest ++ " undecillion "
	| n == 13 && z		= rest
	| n == 13			= rest ++ " duodecillion "
	| n == 14 && z		= rest
	| n == 14			= rest ++ " tredecillion "
	| n == 15 && z		= rest
	| n == 15			= rest ++ " quattuordecillion "
	| n == 16 && z		= rest
	| n == 16			= rest ++ " quindecillion "
	| n == 17 && z		= rest
	| n == 17			= rest ++ " sexdecillion "
	| n == 18 && z		= rest
	| n == 18			= rest ++ " septendecillion "
	| n == 19 && z		= rest
	| n == 19			= rest ++ " octodecillion "
	| n == 20 && z		= rest
	| n == 20			= rest ++ " novemdecillion "
	| n == 21 && z		= rest
	| n == 21			= rest ++ " vigintillion "
	| otherwise			= ""
    where rest = if x /= "zero" then (sayList (n+1) xs) ++ x else sayList (n+1) xs
          z = x == "zero"
------------------------------------------------------------------------------------
		  
-- sayNum function	--
	  
sayNum :: Int -> String
sayNum x = sayList 0 (map sayGroup (sectionList (convertToList x)))

--------------------------------------------------------------------



















