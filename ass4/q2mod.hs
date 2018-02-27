validp :: Int -> Int -> String -> [String]
validp x y xs
  | x == 0 && y == 0 = [xs]
  | x < 0 || y < 0 || x > y = []
  | otherwise = (validp (x-1) y (xs ++ "(")) ++ validp x (y-1) (xs ++ ")")

genWBPS :: Int -> [String]
genWBPS x = validp x x ""
