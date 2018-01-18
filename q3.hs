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
  | xs == []  = True
  | otherwise = False

belongsTo :: Integer -> [Integer] -> Bool
belongsTo x xs
  | xset == []     = False
  | x == head xset = True
  | otherwise = belongsTo x $ tail xset
  where xset = makeSet xs

-- TODO Output in the form of a set (unique and sorted) ?
myInsert :: Integer -> [Integer] -> [Integer]
myInsert x xs = makeSet $ xs ++ [x]

myUnion :: [Integer] -> [Integer] -> [Integer]
myUnion xs ys = makeSet $ xs ++ ys

myIntersection :: [Integer] -> [Integer] -> [Integer]
myIntersection xs ys
  | xset == [] || yset == [] = []
  | xhead == yhead           = [xhead] ++ myIntersection xtail ytail
  | otherwise                = myIntersection xtail ytail
  where xset = makeSet xs
        yset = makeSet ys
        xtail = tail xset
        xhead = head xset
        ytail = tail yset
        yhead = head yset

mySetDifference :: [Integer] -> [Integer] -> [Integer]
mySetDifference xs ys
  | xset == [] = []
  | yset == [] = xset
  | xhead == yhead = mySetDifference xtail ytail
  | otherwise = xhead : mySetDifference xtail ytail
  where xset = makeSet xs
        yset = makeSet ys
        xtail = tail xset
        xhead = head xset
        ytail = tail yset
        yhead = head yset

-- powerset without using foldr
myPowerset :: [Integer] -> [[Integer]]
myPowerset xs = foldl (\acc x -> acc ++ [x]:map (++ [x]) acc) [] $ makeSet xs
-- myPowerset xs =

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
myPowerset2 xs = foldr (\x acc -> ([x]:map (x:) acc) ++ acc) [] $ makeSet xs
