data Tree a
    = Node (Tree a)
           a
           (Tree a)
    | Null
    deriving (Show)

dia :: Tree a -> Int
dia Null = 0
dia (Node lt k rt) = max (max (dia lt) (dia rt)) (maxDepth lt + maxDepth rt + 1)

maxDepth :: Tree a -> Int
maxDepth Null = 0
maxDepth (Node lt k rt) = max (maxDepth lt) (maxDepth rt) + 1
