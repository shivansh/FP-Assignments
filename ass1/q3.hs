-------------------------------------------------------------------------------
-- Library: Do not Modify --

-- Use the following helper functions to generate set from an arbitrary list of
-- integers:

-- sort a list
-- TODO Error in makeSet when type is not specified (Eq /= Ord)
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
  | xs == []  = True
  | otherwise = False


-- TODO Stop at first match.
belongsTo :: Integer -> [Integer] -> Bool
belongsTo x xs
  | [ y | y <- makeSet xs, x == y ] == [] = False
  | otherwise = True


-- TODO Output in the form of a set (unique and sorted) ?
myInsert :: Integer -> [Integer] -> [Integer]
myInsert x xs = makeSet $ x:xs


myUnion :: [Integer] -> [Integer] -> [Integer]
myUnion xs ys = makeSet $ xs ++ ys


myIntersection :: [Integer] -> [Integer] -> [Integer]
myIntersection xs ys = makeSet [ x | x <- xs, y <- ys, x == y ]


mySetDifference :: [Integer] -> [Integer] -> [Integer]
mySetDifference xs ys
  | xs == [] = []
  | ys == [] = xs
  | [ y | y <- ys, xhead == y ] == [] = xhead : mySetDifference xtail ys
  | otherwise = mySetDifference xtail ys
  where xhead = head xs
        xtail = tail xs


-- powerset without using foldr
-- TODO Avoid using extra function.
myPowerset :: [Integer] -> [[Integer]]
myPowerset xs = [] : myPowersetl xs

myPowersetl :: [Integer] -> [[Integer]]
myPowersetl xs = foldl (\acc x -> acc ++ [x]:map (++ [x]) acc) [] $ makeSet xs


-- powerset using foldr
-- A verbose expansion of the function myPowerset2 -
-- prependAll prepends an integer to every list in a list of lists
--      > prependAll :: Integer -> [[Integer]] -> [[Integer]]
--      > prependAll x = map (x:)
--
--      > myPowerset2 :: [Integer] -> [[Integer]]
--      > myPowerset2 = foldr (\x acc -> ([x]:prependAll x acc) ++ acc) []
--
-- TODO Avoid using extra function.
myPowerset2 :: [Integer] -> [[Integer]]
myPowerset2 xs = [] : myPowersetr xs

myPowersetr :: [Integer] -> [[Integer]]
myPowersetr xs = foldr (\x acc -> ([x]:map (x:) acc) ++ acc) [] $ makeSet xs
