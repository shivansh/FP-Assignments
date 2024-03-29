data Peano = Zero | Succ Peano deriving Show

int2Peano :: Integer -> Peano
int2Peano 0 = Zero
int2Peano n = Succ (int2Peano (n-1))

peano2Int :: Peano -> Integer
peano2Int Zero = 0
peano2Int (Succ n) = 1 + peano2Int n

instance Integral Peano where
    quotRem a b = (int2Peano (quot (peano2Int a) (peano2Int b)), int2Peano (rem (peano2Int a) (peano2Int b)))
    toInteger = peano2Int

instance Real Peano where
    toRational = toRational . peano2Int

instance Enum Peano where
    fromEnum = fromIntegral . peano2Int
    toEnum = int2Peano . toInteger

instance Num Peano where
    a + b = int2Peano $ peano2Int a + peano2Int b
    a * b = int2Peano $ peano2Int a * peano2Int b
    a - b
      | peano2Int a < peano2Int b = Zero
      | otherwise = int2Peano $ peano2Int a - peano2Int b
    abs a = a
    signum a
      | peano2Int a == 0 = 0
      | otherwise = Succ 0
    fromInteger = int2Peano
    negate a = Zero - a

instance Ord Peano where
    a <= b = peano2Int a <= peano2Int b

instance Eq Peano where
    a == b = peano2Int a == peano2Int b
