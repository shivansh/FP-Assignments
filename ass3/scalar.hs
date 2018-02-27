data Scalar = Scalar Integer deriving Show

instance Num Scalar where
    (Scalar i) + (Scalar j) = Scalar (i+j)

addNumToScalar :: Integer -> Scalar -> Scalar
addNumToScalar x (Scalar y) = Scalar (x+y)
