-------------------------------------------------------------------------------
-- Library: Do not Modify --

-- Use the following helper functions to generate set from an arbitrary list of
-- integers:

-- sort a list
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


belongsTo :: Integer -> [Integer] -> Bool


myInsert :: Integer -> [Integer] -> [Integer]


myUnion :: [Integer] -> [Integer] -> [Integer]


myIntersection :: [Integer] -> [Integer] -> [Integer]


mySetDifference :: [Integer] -> [Integer] -> [Integer]


-- powerset without using foldr
myPowerset :: [Integer] -> [[Integer]]


-- powerset using foldr
myPowerset2 :: [Integer] -> [[Integer]]
