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

data Matrix a = Matrix [[a]] deriving (Eq, Show)
instance Num a => Num (Matrix a)

matrixAdd :: Matrix a -> Matrix a -> Matrix a
matrixAdd x y = y


-- matrixAdd :: Matrix a -> Matrix a -> Matrix a
-- matrixAdd (x:xs) (y:ys)
--     | xs == []  = vectorAdd x y : []
--     | otherwise = vectorAdd x y : matrixAdd xs ys
    


-- vectorAdd :: Vector a -> Vector a -> Vector a
-- vectorAdd (x:xs) (y:ys)
--     | xs == []  = x+y : []
--     | otherwise = x+y : vectorAdd xs ys
    