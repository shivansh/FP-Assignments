-- pascalSucc evaluates the successor of given list in the pascal's triangle.
pascalSucc :: [Integer] -> [Integer]
pascalSucc xs = 1 : [ a+b | (a,b) <- zip (init xs) (tail xs) ] ++ [1]

pascal = [1] : [ pascalSucc xs | xs <- pascal ]
