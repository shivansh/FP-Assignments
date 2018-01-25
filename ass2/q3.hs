errno :: String -> Int
errno = length . unbalanced

-- unbalanced returns a list of chars which needs to
-- be removed to obtain a balanced parentheses string.
unbalanced :: String -> String
unbalanced = foldl f []
  where f acc x
          | null acc = x:acc
          | (head acc, x) == ('(', ')') = tail acc
          | otherwise = x:acc
