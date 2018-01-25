-- errno returns a list of chars which need to be removed
-- to obtain a well balanced parentheses string (WBPS).
-- TODO: Change signature to String -> Int
errno :: String -> String
errno = foldl (\acc x ->
                if x == '(' then acc ++ [x]
                else if length acc == 0 then acc ++ [x]
                else if last acc == '(' then init acc
                else acc ++ [x]) []

errno' :: String -> Int
errno' xs
  | xs == [] = 0
  | head xs == '(' && last xs == ')' = errno' $ init $ tail xs
  | otherwise = 1 + min (errno' (init xs)) (errno' (tail xs))

-- (()
-- ))())
