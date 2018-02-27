import Data.List  -- DO NOT REMOVE

-- top level function must be called
-- genWBPS

fac :: Int -> Int
fac 0 = 1
fac x = x * fac (x-1)

-- unbalanced returns a list of chars which needs to
-- be removed to obtain a balanced parentheses string.
unbalanced :: String -> String
unbalanced = foldl f []
  where f ('(':xs) ')' = xs
        f xs x = x:xs

uniqify []   = []
uniqify [x]  = [x]
uniqify (x:y:rest) | x == y    =   uniqify (y:rest)
                   | otherwise = x:uniqify (y:rest)

genPS :: Int -> String
genPS x = take n (repeat '(') ++ take n (repeat ')')
  where n = div (fac x) 2

wBPS :: Int -> [String]
wBPS x = foldl f [] xs
  where xs = permutations (genPS x)
        f acc y
          | head y == ')' = acc
          | unbalanced y == [] = y : acc
          | otherwise = acc

genWBPS :: Int -> [String]
genWBPS x = uniqify (sort (wBPS x))
