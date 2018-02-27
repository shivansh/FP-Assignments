data ETree = Val Int
           | Uop Char ETree
           | Bop Char ETree ETree
         deriving Show

interpret :: ETree -> Int
