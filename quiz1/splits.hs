import Data.List

split :: [Int] -> [([Int], [Int])]
split xs = foldl f [([],[])] xs
    where f acc x = ((fst (head acc)) ++ [x], []) : drop 1 (acc ++ [(fst (head acc), xs \\ (fst (head acc)))])
