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
instance Num a => Num (Vector a)
--

--
data Matrix a = Matrix [[a]] deriving (Eq, Show)
instance Num a => Num (Matrix a)
--

-- test functions
matrixAdd :: Matrix a -> Matrix a -> Matrix a
matrixAdd x y = y


vectorAdd :: Vector a -> Vector a -> [a]
vectorAdd (Vector (x:xs)) (Vector (y:ys))
    | length xs == 0 = x+y : []
    | otherwise = x+y : vectorAdd (Vector xs) (Vector ys)


-- vectorAdd :: Vector a -> Vector a -> Vector a
-- vectorAdd vec1 vec2
--     | vec1 == Vector [] = []
--     | otherwise  = (head vec1 + head vec2) : vectorAdd (tail vec1) (tail vec2)
