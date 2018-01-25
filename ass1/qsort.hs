-- The top level function must be called quicksort.
-- you can use other functions
quicksort :: Ord a => [a] -> [a]
-- quicksort
quicksort [] = []
quicksort (x:xs) = quicksort [ i | i <- xs, i < x ]
                   ++ [x] ++
                   quicksort [ i | i <- xs, i >= x ]
