-- hw3.hs
-- Author: Jordan Zalaha
-- Email: jszalaha@ucsc.edu
-- Date: February 8, 2016

data BST k v = Empty | Node k v (BST k v) (BST k v)
				
-- 1.
val :: BST k v -> Maybe v
val Empty = Nothing
val (Node kk vv lt rt) = Just vv

-- 2.
size :: BST k v -> Int
size Empty = 0
size (Node kk vv lt rt) = 1 + (size lt) + (size rt)

-- 3.
ins :: (Ord k) => k -> v -> BST k v -> BST k v
ins key value Empty = Node key value Empty Empty
ins key value (Node kk vv lt rt)
	| key < kk	= Node kk vv (ins key value lt) rt
	| key > kk	= Node kk vv lt (ins key value rt)
	| key == kk	= Node kk value lt rt
	
-- 4.
instance (Show v) => Show (BST k v) where
	show Empty = ""
	show (Node k v lt rt) = show lt ++ " " ++ show v ++ " " ++ show rt
	
-- 5.
data JSON = JStr String
		| JNum Double
		| JArr [JSON]
		| JObj [(String, JSON)]
		
instance Show JSON where
	show (JStr str) 		= show str
	show (JNum db) 			= show db
	show (JArr xs)			= "[" ++ show (head xs) ++ "," ++ show (tail xs) ++ "]"
	show (JObj ((x,y):xs))	= "{" ++ x ++ ":" ++ show y ++ "," ++ show xs ++ "}"
	
-- 6. INCOMPLETE
--class Json a where
--	toJson :: a -> JSON
--	fromJson :: JSON -> a
--instance Json Double where
--	toJson d = JNum d
--	fromJson (JNum j) = j
--instance (Json a) => Json [a] where
--	toJson [] = JArr []
--	toJson (h:t) = JArr [h:(toJson t)]
--	fromJson (JArr (h:t)) = (fromJson h):(fromJson (JArr t))