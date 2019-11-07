--

--
-- vector creation
--

{-# LANGUAGE GADTs #-}
data Vector a where
    Vector :: Num a => [a] -> Vector a

xVector :: Int -> a -> Vector a
xVector dim val
    | dim == 0  = []
    | otherwise = val : xVector (dim - 1) val