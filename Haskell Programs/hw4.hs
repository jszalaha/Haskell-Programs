-- hw4.hs
-- Author: Jordan Zalaha
-- Email: jszalaha@ucsc.edu
-- Date: 2/28/2016

{-# LANGUAGE FlexibleInstances, OverlappingInstances, UndecidableInstances #-}
import System.Random
import Data.Int

--------------------------------------------------
-- 1.

class (Show a) => Gen a where
	gen :: IO a
	
instance (Show a, Random a) => Gen a where
	gen = randomIO
	
instance (Gen a, Gen b) => Gen (a, b) where
	gen = do
		x <- gen
		y <- gen
		return (x, y)
	
instance (Gen a) => Gen [a] where
	gen = do
		list <- (sequence [gen | _ <- [1..10]])
		rand <- newStdGen
		let (len, _) = (randomR (0, 10) rand)
		return $ take len list
		
--------------------------------------------------
-- 2, 3.

class Testable a where
	test :: String -> a -> IO (Bool, String)

instance Testable Bool where
	test s b = return (b, s)

instance (Gen a, Testable b) => Testable (a -> b) where 
	test s f = do 
			n <- gen
			test (s ++ " " ++ show n) (f n)
			
--------------------------------------------------
-- 3, 4. 

quickCheck :: (Testable a) => Int -> a -> IO ()
quickCheck 0 _ = return ()
quickCheck n t = do
				(b, s) <- test "" t
				if b == True
				then quickCheck (n-1) t
				else putStrLn $ "Failing inputs = " ++ s
				
--------------------------------------------------
-- 5, 6.

-- This sorting function was originally missing the x's
-- following the inserts which caused it to do some
-- funky stuff.
isort :: [Int8] -> [Int8]
isort [] = []
isort (x:xs) = insert x (isort xs)
	where
		insert x [] = [x]
		insert x (h:t)
			| x > h = h:(insert x t)
			| x <= h = x:h:t

-- In the list comprehension describing the lesser half 
-- of the sorted result, one of the filters was (a < x)
-- which would cause an issue if a was ever equal to x.
-- It was fixed by changing it to (a <= x).
qsort :: [Int8] -> [Int8]
qsort [] = []
qsort (x:xs) = qsort [a | a <- xs, a <= x] ++ [x] ++ qsort [a | a <- xs, a > x]

testSort :: ([Int8] -> [Int8]) -> [Int8] -> [Int8] -> Bool
testSort srt lst lst2 =  (prop_inc srt lst) && 
					(prop_idenp srt lst) && 
					(prop_max srt lst) &&
					(prop_min srt lst lst2)

-- PROPERTIES OF A SORTED LIST -----------------------------
-- all elements increasing in value from head to tail
prop_inc :: ([Int8] -> [Int8]) -> [Int8] -> Bool
prop_inc srt xs = isIncreasing (srt xs)
	where
		isIncreasing []		  = True
		isIncreasing [x] 	  = True
		isIncreasing (x:y:xs) = x <= y && isIncreasing (y:xs)

-- sorting an already sorted list returns the sorted list
prop_idenp :: ([Int8] -> [Int8]) -> [Int8] -> Bool
prop_idenp srt xs = srt (srt xs) == (srt xs) 

-- the last element in the list is the maximum
prop_max :: ([Int8] -> [Int8]) -> [Int8] -> Bool
prop_max srt xs = (null xs) || (last (srt xs) == maximum xs)

-- the first element in this list is the minimum
-- even when the list being sorted is a second list
-- appended onto the first. the head is the minimum 
-- between both.
prop_min :: ([Int8] -> [Int8]) -> [Int8] -> [Int8] -> Bool
prop_min srt xs ys = (null xs) || ( (null ys) || head (srt (xs ++ ys)) == min (minimum xs) (minimum ys) )
-------------------------------------------------------------