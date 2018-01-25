errno :: String -> Int
errno = length . unbalanced

-- unbalanced returns a list of chars which needs to
-- be removed to obtain a balanced parentheses string.
unbalanced :: String -> String
unbalanced = foldl (\acc x -> if length acc == 0 then x:acc
                         else if head acc == '(' && x == ')' then tail acc
                         else x:acc) []
