--

--
-- vector creation
--

-- {-# LANGUAGE GADTs #-}
-- data Vector a where
--     Vector :: Num a => [a] -> Vector a

-- type Vector (a :: Eq) = [a]

-- xVector :: Int -> a -> Vector a
-- xVector dim val
--     | dim == 0  = []
--     | otherwise = val : xVector (dim - 1) val
    
--
-- create vector type
--
data Vector a = Vector [a] deriving (Eq, Show)

vectorAdd :: Num a => [a] ->  [a] -> [a]
vectorAdd x y = zipWith (+) x y

vecAbs :: (Ord a,Num a) => [a] ->  [a]
vecAbs [] = []
vecAbs (x:xs)
    | x < 0     = -x : vecAbs xs
    | otherwise = x : vecAbs xs

instance (Ord a,Num a) => Num (Vector a)
    where 
        (Vector a) + (Vector b) = Vector $ vectorAdd a b
        (Vector a) - (Vector b) = Vector $ zipWith (-) a b
        (Vector a) * (Vector b) = Vector $ zipWith (*) a b
        -- negate (Vector a)       = Vector $ zipWith (-) (zero (length a)) a
        abs (Vector a)          = Vector $ vecAbs a
        signum                  = undefined
        fromInteger             = undefined

--

--
-- data Matrix a = Matrix [[a]] deriving (Eq, Show)
-- instance Num a => Num (Matrix a)
--

-- test functions
-- matrixAdd :: Matrix a -> Matrix a -> Matrix a
-- matrixAdd x y = y



-- vectorAdd :: Vector a -> Vector a -> Vector a
-- vectorAdd vec1 vec2
--     | vec1 == Vector [] = []
--     | otherwise  = (head vec1 + head vec2) : vectorAdd (tail vec1) (tail vec2)
