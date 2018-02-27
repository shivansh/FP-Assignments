-- top level function must be called coinChange
-- use as many helper functions as necessary

-- Main routine.
coinChange :: Int -> [Int] -> [Int]
coinChange d xs = newList xs $ shortest $ powerChange d xs

-- Prints output in the expected format.
newList :: [Int] -> [Int] -> [Int]
newList coins xs = [ count y xs | y <- coins ]

-- Count the occurrences of an integer in a list.
count :: Int -> [Int] -> Int
count x = foldl f 0
  where f acc y
          | x == y = acc + 1
          | otherwise = acc

-- Find the shortest list from the list of lists.
shortest :: [[Int]] -> [Int]
shortest [] = []
shortest xss = foldl f (head xss) (tail xss)
  where f acc xs
          | length xs < length acc = xs
          | otherwise = acc

powerChange :: Int -> [Int] -> [[Int]]
powerChange _ [] = []
powerChange 0 _ = []
powerChange d (x:xs) = foldl f [] (x:xs)
  where f acc y
          | temp == [] = []
          | d-y >= 0 = shortest (map (y:) (temp : (powerChange (d-y) xs))) : acc
          | otherwise = []
          where temp = setChange (d-y) (x:xs)

setChange :: Int -> [Int] -> [Int]
setChange d [] = []
setChange d (x:xs)
  | d-x == 0 = [x]
  | d-x < 0 || null result = setChange d xs
  | otherwise = x : result
  where result = setChange (d-x) (x:xs)
