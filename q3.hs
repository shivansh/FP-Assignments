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

mySetDifference :: [Integer] -> [Integer] -> [Integer]
mySetDifference xs ys
  | ys == [] = xs
  | head xs == head ys = mySetDifference (tail xs) (tail ys)
  | otherwise = head xs : mySetDifference (tail xs) (tail ys)

-- powerset without using foldr
myPowerset :: [Integer] -> [[Integer]]
myPowerset = foldl (\acc x -> acc ++ [x]:map (++ [x]) acc) []

-- powerset using foldr
-- A verbose expansion of the function myPowerset2 -
-- prependAll prepends an integer to every list in a list of lists
--      > prependAll :: Integer -> [[Integer]] -> [[Integer]]
--      > prependAll x = map (x:)
--
--      > myPowerset2 :: [Integer] -> [[Integer]]
--      > myPowerset2 = foldr (\x acc -> ([x]:prependAll x acc) ++ acc) []
--
-- TODO Add empty set.
myPowerset2 :: [Integer] -> [[Integer]]
myPowerset2 = foldr (\x acc -> ([x]:map (x:) acc) ++ acc) []
