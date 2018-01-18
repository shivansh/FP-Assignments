-------------------------------------------------------------------------------
-- Library: Do not Modify --

-- Use the following helper functions to generate set from an arbitrary list of
-- integers:

-- sort a list
-- TODO The type needs to be specified here??
sort :: [Integer] -> [Integer]
sort []     = []
sort (x:xs) = sort (filter (< x)  xs) ++ [x] ++ sort (filter (>= x) xs)

-- removes consecutive duplicated elements from a list
uniqify []   = []
uniqify [x]  = [x]
uniqify (x:y:rest) | x == y    =   uniqify (y:rest)
                   | otherwise = x:uniqify (y:rest)

-- make set from a general list
makeSet = uniqify . sort
-------------------------------------------------------------------------------
isEmpty :: [Integer] -> Bool
isEmpty xs
  | xs == [] = True
  | otherwise = False

belongsTo :: Integer -> [Integer] -> Bool
belongsTo x xs
  | xs == [] = False
  | x == head xs = True
  | otherwise = belongsTo x $ tail xs

-- TODO Output in the form of a set (unique and sorted) ?
myInsert :: Integer -> [Integer] -> [Integer]
myInsert x xs = makeSet $ xs ++ [x]

myUnion :: [Integer] -> [Integer] -> [Integer]
myUnion xs ys = makeSet $ xs ++ ys

-- TODO Assumes input in form of set
myIntersection :: [Integer] -> [Integer] -> [Integer]
-- FIXME
myIntersection = mySetIntersection

mySetIntersection :: [Integer] -> [Integer] -> [Integer]
mySetIntersection xs ys
  | xs == [] || ys == [] = []
  | head xs == head ys = [head xs] ++ mySetIntersection (tail xs) (tail ys)
  | otherwise = mySetIntersection xs ys

-- mySetDifference :: [Integer] -> [Integer] -> [Integer]


-- powerset without using foldr
-- myPowerset :: [Integer] -> [[Integer]]


-- powerset using foldr
--
-- Appends an integer to every list in a list of list
appendAll :: Integer -> [[Integer]] -> [[Integer]]
appendAll x xs = (map (++ [x]) xs) ++ [[x]]

myPowerset2 :: [Integer] -> [[Integer]]
myPowerset2 = foldl (\acc x -> acc ++ appendAll x acc) []
