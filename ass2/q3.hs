errno :: String -> Int
errno = length . unbalanced

-- unbalanced returns a list of chars which needs to
-- be removed to obtain a balanced parentheses string.
unbalanced :: String -> String
unbalanced = foldl f []
  where f ('(':xs) ')' = xs
        f xs x = x:xs
