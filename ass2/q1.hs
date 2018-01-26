-- top level function must be called coinChange
-- use as many helper functions as necessary

-- signature for coinChange
coinChange :: Int -> [Int] -> [Int]
coinChange d [] = []
coinChange d (x:xs)
  | d-x == 0 = [x]
  | d-x < 0 || null (coinChange (d-x) (x:xs)) = coinChange d xs
  | otherwise = x : coinChange (d-x) (x:xs)
