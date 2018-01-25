-- errno returns a list of chars which need to be removed
-- to obtain a well balanced parentheses string (WBPS).
-- TODO: Change signature to String -> Int
errno :: String -> String
errno = foldl (\acc x -> if length acc == 0 then x:acc
                         else if head acc == '(' && x == ')' then tail acc
                         else x:acc) []
