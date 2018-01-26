-- top level function must be called setChange
-- use as many helper functions as necessary

-- Main routine.
coinChange :: Int -> [Int] -> [Int]
coinChange d xs = shortest (powerChange d xs)

-- Find the shortest list from the list of lists.
shortest :: [[Int]] -> [Int]
shortest [x] = x
shortest (x:y:list)
  | length x < length y = shortest (x:list)
  | otherwise = shortest (y:list)

powerChange :: Int -> [Int] -> [[Int]]
powerChange _ [] = []
powerChange 0 _ = []
powerChange d (x:xs) = foldl f [] (x:xs)
  where f acc y
          | d-y >= 0 = map (y:) (setChange (d-y) (x:xs) : (powerChange (d-y) (x:xs))) ++ acc
          | otherwise = []

setChange :: Int -> [Int] -> [Int]
setChange d [] = []
setChange d (x:xs)
  | d-x == 0 = [x]
  | d-x < 0 || null (setChange (d-x) (x:xs)) = setChange d xs
  | otherwise = x : setChange (d-x) (x:xs)  -- TODO avoid same recursive calls twice
