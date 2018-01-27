-- top level function must be called coinChange
-- use as many helper functions as necessary

-- Main routine.
coinChange :: Int -> [Int] -> [Int]
coinChange d xs = shortest (powerChange d xs)

-- Find the shortest list from the list of lists.
shortest :: [[Int]] -> [Int]
shortest [] = []
shortest [x] = x
shortest (x:y:list)
  | length x < length y = shortest (x:list)
  | otherwise = shortest (y:list)

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
